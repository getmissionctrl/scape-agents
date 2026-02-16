-- scape/app/Agent.hs
-- Main entry point for scape-agent executable
--
-- The agent runs inside a Firecracker microVM and:
-- 1. Fetches config from MMDS (instance ID, NATS credentials)
-- 2. Connects to NATS server for command/observation messaging
-- 3. Runs HTTP server for health checks and metrics
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race, race_)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, getCurrentTime)
import Katip (Severity(..), LogStr, LogEnv, logStr)
import Options.Applicative hiding (command)
import Prelude hiding (id)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)

import Scape.Agent.Executor
  ( executeCommandWithCallback
  , OutputCallback
  )
import Scape.Agent.Logging
  ( initAgentEnv
  , closeAgentEnv
  , logInfoIO
  , logWarnIO
  , aeLogEnv
  , Namespace(..)
  )
import Scape.Agent.MMDS (fetchMMDSConfig, MMDSConfig (..), NatsConfig (..))
import Scape.Agent.Nats
  ( NatsAgentConfig (..)
  , runNatsAgent
  , ObservationPublisher
  , publishOutputBytes
  , publishComplete
  , publishError
  )
import Scape.Protocol.Observation
  ( Channel(..)
  , Observation(..)
  , FileContentEvent(..)
  , FileWrittenEvent(..)
  , SecretsInjectedEvent(..)
  )
import qualified Scape.Protocol.Control as Ctrl
import Scape.Agent.Server (runServer, ServerConfig(..), defaultServerConfig)
import Scape.Protocol.Command
  ( Command(..)
  , ExecRequest(..)
  )
import qualified Scape.Protocol.Command as PC

-- | Agent version (should match package version)
agentVersion :: Text
agentVersion = "0.1.0"

-- | Command line options
data Options = Options
  { optPort       :: !Int
  , optLogLevel   :: !Severity
  , optNatsUrl    :: !(Maybe String)  -- ^ Override NATS URL (for testing without MMDS)
  , optNatsCreds  :: !(Maybe FilePath)  -- ^ NATS credentials file (for local testing)
  , optInstanceId :: !(Maybe String)  -- ^ Instance ID (for local testing)
  }

-- | Parser for command line options
optionsParser :: Parser Options
optionsParser = Options
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value (scPort defaultServerConfig)
     <> showDefault
     <> help "HTTP server port for health/metrics"
      )
  <*> option severityReader
      ( long "log-level"
     <> short 'l'
     <> metavar "LEVEL"
     <> value InfoS
     <> showDefaultWith showSeverity
     <> help "Log level (debug, info, notice, warning, error)"
      )
  <*> optional (strOption
      ( long "nats-url"
     <> metavar "URL"
     <> help "NATS server URL (overrides MMDS config, for testing)"
      ))
  <*> optional (strOption
      ( long "nats-creds"
     <> short 'c'
     <> metavar "FILE"
     <> help "NATS credentials file (for local testing without MMDS)"
      ))
  <*> optional (strOption
      ( long "instance-id"
     <> short 'i'
     <> metavar "ID"
     <> help "Instance ID (for local testing without MMDS)"
      ))

-- | Parse severity from string
severityReader :: ReadM Severity
severityReader = eitherReader $ \s -> case map toLower s of
  "debug"   -> Right DebugS
  "info"    -> Right InfoS
  "notice"  -> Right NoticeS
  "warning" -> Right WarningS
  "error"   -> Right ErrorS
  _         -> Left $ "Unknown log level: " <> s <> ". Use: debug, info, notice, warning, error"
  where
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

-- | Show severity as string
showSeverity :: Severity -> String
showSeverity = \case
  DebugS     -> "debug"
  InfoS      -> "info"
  NoticeS    -> "notice"
  WarningS   -> "warning"
  ErrorS     -> "error"
  CriticalS  -> "critical"
  AlertS     -> "alert"
  EmergencyS -> "emergency"

-- | Program info
opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Run the Scape agent inside a microVM"
 <> header "scape-agent - MicroVM execution agent"
  )

-- | Helper to create LogStr from Text
logT :: Text -> LogStr
logT = logStr

main :: IO ()
main = do
  options <- execParser opts

  let serverConfig = defaultServerConfig { scPort = optPort options }
      logLevel = optLogLevel options

  -- Initialize environment with logging
  bracket (initAgentEnv (Namespace ["scape-agent"]) logLevel) closeAgentEnv $ \env -> do
    startTime <- getCurrentTime
    let logEnv = aeLogEnv env

    logInfoIO logEnv (Namespace ["main"]) $ logT "Agent starting"
    logInfoIO logEnv (Namespace ["main"]) $ logT $ "Version: " <> agentVersion

    -- Try to fetch config from MMDS (2 second timeout)
    mmdsResult <- fetchMMDSConfig logEnv 2000

    -- Build NATS config from MMDS or CLI override
    mNatsConfig <- case (mmdsResult, optNatsUrl options, optNatsCreds options, optInstanceId options) of
      (Just mmds, _, _, _) -> do
        -- Use MMDS config (production mode in VM)
        logInfoIO logEnv (Namespace ["main"]) $ logT $ "MMDS instance-id: " <> mmds.instanceId
        logInfoIO logEnv (Namespace ["main"]) $ logT $ "NATS URL: " <> mmds.nats.url
        -- Log credential content info (not the actual content for security)
        case mmds.nats.credsFileContent of
          Nothing -> logWarnIO logEnv (Namespace ["main"]) $ logT "credsFileContent is Nothing - agent may fail to connect!"
          Just content -> do
            let contentLen = T.length content
                lineCount = length (T.lines content)
            logInfoIO logEnv (Namespace ["main"]) $ logT $ "Creds received: " <> showT contentLen <> " bytes, " <> showT lineCount <> " lines"
        pure $ Just $ mmdsToNatsConfig mmds
      (Nothing, Just natsUrl, mCreds, mInstId) -> do
        -- CLI override for testing without MMDS
        let instId = maybe "test-agent" T.pack mInstId
        logWarnIO logEnv (Namespace ["main"]) $ logT "Using CLI NATS config (no MMDS)"
        logInfoIO logEnv (Namespace ["main"]) $ logT $ "Instance ID: " <> instId
        logInfoIO logEnv (Namespace ["main"]) $ logT $ "NATS URL: " <> T.pack natsUrl
        pure $ Just $ testNatsConfig (T.pack natsUrl) instId mCreds
      (Nothing, Nothing, _, _) -> do
        logWarnIO logEnv (Namespace ["main"]) $ logT "No NATS config available (no MMDS, no --nats-url)"
        logWarnIO logEnv (Namespace ["main"]) $ logT "Agent will only run HTTP server"
        pure Nothing

    logInfoIO logEnv (Namespace ["main"]) $ logT $ "HTTP port: " <> showT (optPort options)

    -- Run NATS agent and HTTP server concurrently
    case mNatsConfig of
      Just natsCfg -> do
        logInfoIO logEnv (Namespace ["main"]) $ logT "Starting NATS agent"
        race_
          (natsWithReconnect logEnv options natsCfg agentVersion startTime)
          (runServer env startTime serverConfig)
      Nothing -> do
        -- No NATS config - just run HTTP server
        logInfoIO logEnv (Namespace ["main"]) $ logT "Running in HTTP-only mode"
        runServer env startTime serverConfig

-- | Convert MMDS config to NatsAgentConfig
mmdsToNatsConfig :: MMDSConfig -> NatsAgentConfig
mmdsToNatsConfig mmds =
  let nc :: NatsConfig
      nc = mmds.nats
  in NatsAgentConfig
       mmds.instanceId         -- instanceId
       nc.url                  -- natsUrl
       nc.user                 -- natsUser
       nc.cmdSubject           -- cmdSubject
       nc.obsPrefix            -- obsPrefix
       Nothing                 -- credsFile (for local testing only)
       nc.credsFileContent     -- credsFileContent (raw .creds from MMDS)
       nc.announceSubject      -- announceSubject (for template builds)

-- | Run NATS agent with MMDS-aware reconnection after snapshot resume
--
-- On snapshot resume, the MMDS instanceId changes. This function polls MMDS
-- every second and races against the running NATS agent. When the instanceId
-- changes, the old NATS connection is torn down and a new one is established
-- with the updated config.
natsWithReconnect :: LogEnv -> Options -> NatsAgentConfig -> Text -> UTCTime -> IO ()
natsWithReconnect logEnv options cfg version startTime = go cfg
  where
    go currentCfg = do
      shutdownVar <- newEmptyMVar
      result <- race
        (runNatsAgent currentCfg version startTime (optPort options)
           (handleCommand logEnv) (takeMVar shutdownVar))
        (watchMMDS logEnv currentCfg.instanceId)
      case result of
        Left ()      -> pure ()  -- Normal shutdown
        Right newCfg -> do
          logInfoIO logEnv (Namespace ["nats"]) $
            logT $ "MMDS config changed (" <> currentCfg.instanceId
                <> " -> " <> newCfg.instanceId <> "), reconnecting NATS..."
          go newCfg

-- | Poll MMDS until instanceId changes (snapshot resume detection)
watchMMDS :: LogEnv -> Text -> IO NatsAgentConfig
watchMMDS logEnv currentInstanceId = do
  threadDelay 1000000  -- 1 second
  mmdsResult <- fetchMMDSConfig logEnv 2000
  case mmdsResult of
    Just mmds | mmds.instanceId /= currentInstanceId ->
      pure (mmdsToNatsConfig mmds)
    _ -> watchMMDS logEnv currentInstanceId

-- | Create test config for CLI override
testNatsConfig :: Text -> Text -> Maybe FilePath -> NatsAgentConfig
testNatsConfig url instId mCreds = NatsAgentConfig
  instId                         -- instanceId
  url                            -- natsUrl
  ""                             -- natsUser (not used with creds file)
  ("scape.cmd." <> instId)       -- cmdSubject
  ("scape.obs." <> instId)       -- obsPrefix
  mCreds                         -- credsFile (for local testing)
  Nothing                        -- credsFileContent (not used for local testing)
  Nothing                        -- announceSubject (not needed for testing)

-- | Handle incoming commands from NATS
--
-- This is called for each command received on the agent's command subject.
-- It dispatches to the appropriate handler based on command type.
-- The publisher is passed through so handlers can send observations back.
-- The reannounce callback is used to re-publish Ready after VM resume.
handleCommand :: LogEnv -> ObservationPublisher -> IO () -> Command -> IO ()
handleCommand logEnv publish reannounce cmd = do
  -- Log the received command
  logInfoIO logEnv ns $ logT $ "Received command: " <> cmdSummary cmd
  case cmd of
    CmdExec req -> handleExec logEnv publish req
    CmdCancel cmdId -> do
      logWarnIO logEnv ns $ logT $ "Cancel not implemented for: " <> showT cmdId
    CmdWriteFile req -> handleWriteFile logEnv publish req
    CmdReadFile req -> handleReadFile logEnv publish req
    CmdInjectSecrets req -> handleInjectSecrets logEnv publish req
    CmdPing -> do
      logInfoIO logEnv ns $ logT "Ping received, re-announcing Ready"
      reannounce
    CmdShutdown -> do
      logInfoIO logEnv ns $ logT "Shutdown requested"
  where
    ns = Namespace ["command"]

-- | Get a summary of a command for logging
cmdSummary :: Command -> Text
cmdSummary = \case
  CmdExec req -> "Exec: " <> req.command <> " " <> T.unwords req.args
  CmdCancel cmdId -> "Cancel: " <> showT cmdId
  CmdWriteFile req -> "WriteFile: " <> T.pack req.path
  CmdReadFile req -> "ReadFile: " <> T.pack req.path
  CmdInjectSecrets _ -> "InjectSecrets"
  CmdPing -> "Ping"
  CmdShutdown -> "Shutdown"

-- | Handle exec command
--
-- Runs the command and publishes output observations via NATS.
-- The publisher is used to send output, complete, and error observations
-- back to the orchestrator.
handleExec :: LogEnv -> ObservationPublisher -> ExecRequest -> IO ()
handleExec logEnv publish req = do
  logInfoIO logEnv ns $ logT $ "Executing: " <> req.command <> " in " <> T.pack req.workdir
  -- Convert output channel and bytes to NATS observation
  let onOutput :: OutputCallback
      onOutput channel bytes = do
        -- Convert Protocol.Control.OutputChannel to Protocol.Observation.Channel
        let natsChannel = case channel of
              Ctrl.Stdout -> Stdout
              Ctrl.Stderr -> Stderr
        publishOutputBytes publish (req.id) natsChannel bytes

      onComplete exitCode duration =
        publishComplete publish (req.id) exitCode duration

      onError msg =
        publishError publish (Just req.id) msg

      mStdin = case req.stdin of
        Nothing -> Nothing
        Just b64 -> case B64.decode (TE.encodeUtf8 b64) of
          Left _ -> Nothing
          Right bs -> Just bs

  executeCommandWithCallback
    req.id
    req.command
    req.args
    (Map.toList req.env)
    req.workdir
    mStdin
    req.timeout
    onOutput
    onComplete
    onError
  where
    ns = Namespace ["exec"]

-- | Handle write file command
--
-- Writes base64-decoded content to the specified path.
-- Creates parent directories if needed.
handleWriteFile :: LogEnv -> ObservationPublisher -> PC.WriteFileRequest -> IO ()
handleWriteFile logEnv publish req = do
  logInfoIO logEnv ns $ logT $ "Writing file: " <> T.pack req.path
  case B64.decode (TE.encodeUtf8 req.content) of
    Left err -> publishError publish Nothing $ "Base64 decode error: " <> T.pack err
    Right bytes -> do
      let dir = takeDirectory req.path
      createDirectoryIfMissing True dir
      BS.writeFile req.path bytes
      case req.mode of
        Just m -> setFileMode req.path (fromIntegral m)
        Nothing -> pure ()
      publish $ ObsFileWritten FileWrittenEvent
        { path = req.path
        , size = BS.length bytes
        }
  where
    ns = Namespace ["file"]

-- | Handle read file command
--
-- Reads file content and returns as base64-encoded observation.
handleReadFile :: LogEnv -> ObservationPublisher -> PC.ReadFileRequest -> IO ()
handleReadFile logEnv publish req = do
  logInfoIO logEnv ns $ logT $ "Reading file: " <> T.pack req.path
  exists <- doesFileExist req.path
  if not exists
    then publishError publish Nothing $ "File not found: " <> T.pack req.path
    else do
      bytes <- BS.readFile req.path
      let limited = case req.maxBytes of
            Just n  -> BS.take n bytes
            Nothing -> bytes
          encoded = TE.decodeUtf8 $ B64.encode limited
      publish $ ObsFileContent FileContentEvent
        { path = req.path
        , content = encoded
        , size = BS.length limited
        }
  where
    ns = Namespace ["file"]

-- | Handle inject secrets command
--
-- Writes each secret to a file in the target directory with 0600 permissions.
handleInjectSecrets :: LogEnv -> ObservationPublisher -> PC.SecretsRequest -> IO ()
handleInjectSecrets logEnv publish req = do
  logInfoIO logEnv ns $ logT $ "Injecting " <> showT (Map.size req.secrets) <> " secrets"
  createDirectoryIfMissing True req.targetDir
  let secretsList = Map.toList req.secrets
  forM_ secretsList $ \(secretName, secretValue) -> do
    let filePath = req.targetDir </> T.unpack secretName
    TIO.writeFile filePath secretValue
    setFileMode filePath 0o600
  publish $ ObsSecretsInjected SecretsInjectedEvent
    { count = length secretsList
    , targetDir = req.targetDir
    }
  where
    ns = Namespace ["secrets"]

-- | Show as Text
showT :: Show a => a -> Text
showT = fromString . show
