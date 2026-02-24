-- | Agent NATS client module
--
-- Handles NATS connection and message handling for the agent.
-- Replaces the TCP/Vsock control plane with NATS pub/sub.
--
-- The agent:
--   - Reads NatsConfig from MMDS on boot
--   - Connects to NATS server
--   - Subscribes to scape.cmd.<instanceId> for commands
--   - Publishes observations to scape.obs.<instanceId>.<type>
module Scape.Agent.Nats
  ( -- * Connection
    NatsAgentConfig (..)
  , fromMMDSConfig
  , connectAgent
  , runNatsAgent

    -- * Observations (Agent → Orchestrator)
  , publishReady
  , publishOutput
  , publishOutputBytes
  , publishComplete
  , publishError
  , publishMetrics
  , publishShuttingDown

    -- * Announcements (for template builds)
  , ReadyAnnouncement (..)
  , publishReadyAnnouncement
  , discoverIpAddress

    -- * Types
  , ObservationPublisher
  , CommandHandler
  , ReannounceCallback
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (void)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified Net.IPv4 as IPv4
import Network.Info (getNetworkInterfaces, NetworkInterface(..))
import qualified Network.Info as NI
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>))

import Scape.Nats
  ( NatsConnection
  , NatsConfig (..)
  , connectNats
  , disconnectNats
  , publishJson
  , subscribeJson
  )
import qualified Scape.Nats as SN
import Scape.Protocol.Command
  ( Command (..)
  , CommandId
  )
import Scape.Protocol.Observation
  ( Observation (..)
  , AgentInfo (..)
  , OutputEvent (..)
  , CompleteEvent (..)
  , ErrorEvent (..)
  , MetricsEvent (..)
  , Channel (..)
  , IPv4
  )
import qualified Scape.Protocol.MMDS as MMDS
import Scape.Agent.Metrics (metricsLoop)
import Scape.Agent.State (AgentState)

-- | Agent-side NATS configuration
data NatsAgentConfig = NatsAgentConfig
  { instanceId       :: !Text
  , natsUrl          :: !Text
  , natsUser         :: !Text
  , cmdSubject       :: !Text    -- ^ Subject to subscribe for commands
  , obsPrefix        :: !Text    -- ^ Prefix for observation subjects
  , credsFile        :: !(Maybe FilePath)   -- ^ Optional creds file for JWT+NKey auth (for local testing)
  , credsFileContent :: !(Maybe Text)       -- ^ Raw .creds file content (from MMDS)
  , announceSubject  :: !(Maybe Text)       -- ^ Optional subject to announce readiness with IP
  }
  deriving stock (Generic, Show, Eq)

-- | Convert MMDS NatsConfig to agent config
fromMMDSConfig :: Text -> MMDS.NatsConfig -> NatsAgentConfig
fromMMDSConfig instId cfg = NatsAgentConfig
  { instanceId = instId
  , natsUrl = MMDS.url cfg
  , natsUser = MMDS.user cfg
  , cmdSubject = MMDS.cmdSubject cfg
  , obsPrefix = MMDS.obsPrefix cfg
  , credsFile = Nothing                       -- Used for local testing
  , credsFileContent = MMDS.credsFileContent cfg  -- Raw creds from MMDS
  , announceSubject = MMDS.announceSubject cfg
  }

-- | Function type for publishing observations
type ObservationPublisher = Observation -> IO ()

-- | Callback for handling commands
--
-- Receives the observation publisher so command handlers can
-- publish output, complete, and error observations.
-- | Callback to re-publish Ready observation (used after resume)
type ReannounceCallback = IO ()

-- | Command handler receives publisher, reannounce callback, and the command
type CommandHandler = ObservationPublisher -> ReannounceCallback -> Command -> IO ()

-- | Connect to NATS and return connection + publisher
--
-- If credsFileContent is provided (from MMDS), writes it directly to a temp
-- file. This avoids parsing/reconstruction bugs. Otherwise falls back to
-- credsFile for local testing.
connectAgent :: NatsAgentConfig -> IO (NatsConnection, ObservationPublisher)
connectAgent cfg = do
  -- Determine the credentials file to use
  mCredsPath <- case credsFileContent cfg of
    Just content -> do
      -- Write raw creds content directly to temp file
      tmpDir <- getTemporaryDirectory
      let credsDir = tmpDir </> "scape-agent"
          credsPath' = credsDir </> "agent.creds"
      createDirectoryIfMissing True credsDir
      writeFile credsPath' (T.unpack content)
      pure $ Just credsPath'
    Nothing ->
      -- Fall back to existing credsFile if provided (for local testing)
      pure (cfg.credsFile)

  let natsCfg = NatsConfig
        { SN.url = natsUrl cfg
        , SN.user = if T.null (natsUser cfg) then Nothing else Just (natsUser cfg)
        , SN.password = Nothing
        , SN.credsFile = mCredsPath
        }
  conn <- connectNats natsCfg
  let publisher = mkPublisher conn (obsPrefix cfg)
  pure (conn, publisher)

-- | Create an observation publisher for a given prefix
mkPublisher :: NatsConnection -> Text -> ObservationPublisher
mkPublisher conn prefix obs = do
  let (subject, _) = observationSubject prefix obs
  publishJson conn subject obs

-- | Determine the subject for an observation based on type
observationSubject :: Text -> Observation -> (Text, Text)
observationSubject prefix obs = case obs of
  ObsReady _           -> (prefix <> ".ready", "ready")
  ObsOutput _          -> (prefix <> ".output", "output")
  ObsComplete _        -> (prefix <> ".complete", "complete")
  ObsError _           -> (prefix <> ".error", "error")
  ObsMetrics _         -> (prefix <> ".metrics", "metrics")
  ObsFileContent _     -> (prefix <> ".file.content", "file.content")
  ObsFileWritten _     -> (prefix <> ".file.written", "file.written")
  ObsDirListing _      -> (prefix <> ".dir.listing", "dir.listing")
  ObsSecretsInjected _ -> (prefix <> ".secrets", "secrets")
  ObsShuttingDown      -> (prefix <> ".shutdown", "shutdown")

-- | Run the NATS agent connection loop
--
-- This is the main entry point for the agent's NATS communication.
-- It:
--   1. Connects to NATS
--   2. Publishes Ready observation
--   3. If announceSubject is set, announces IP address for orchestrator
--   4. Subscribes to command subject
--   5. Handles commands via the callback
runNatsAgent
  :: NatsAgentConfig
  -> Text           -- ^ Agent version
  -> UTCTime        -- ^ Start time
  -> Int            -- ^ HTTP port for health checks
  -> AgentState     -- ^ Shared agent state (for metrics loop)
  -> CommandHandler -- ^ Command handler callback
  -> IO ()          -- ^ Shutdown signal (e.g., wait on MVar)
  -> IO ()
runNatsAgent cfg version startTime httpPort agentState handleCmd waitShutdown =
  bracket (connectAgent cfg) (disconnectNats . fst) $ \(conn, publish) -> do
    -- Discover IP address first (needed for Ready observation)
    ip <- discoverIpAddress

    -- Create reannounce callback for use after VM resume
    let reannounce = do
          putStrLn "[Agent] Re-announcing Ready after resume"
          publishReady publish cfg.instanceId version startTime ip

    -- Publish Ready observation (standard for all agents)
    publishReady publish cfg.instanceId version startTime ip

    -- If announceSubject is set, announce IP address
    -- This is used during template builds so orchestrator can discover VM IP
    case cfg.announceSubject of
      Nothing -> putStrLn "[Agent] No announceSubject configured"
      Just subject -> do
        putStrLn $ "[Agent] Will announce to subject: " ++ T.unpack subject
        let ipText = IPv4.encode ip
        putStrLn $ "[Agent] Publishing announcement to " ++ T.unpack subject
        publishReadyAnnouncement conn subject cfg.instanceId ipText httpPort
        putStrLn "[Agent] Announcement published"

    -- Start background metrics loop
    void $ forkIO $ metricsLoop agentState (publishMetrics publish)

    -- Subscribe to command subject
    void $ subscribeJson conn cfg.cmdSubject $ \result ->
      case result of
        Left err -> do
          -- Log parse error but don't crash
          publishError publish Nothing ("Failed to parse command: " <> T.pack err)
        Right cmd -> do
          -- Handle command - errors are caught and published
          -- Pass publisher and reannounce callback so handlers can use them
          catch (handleCmd publish reannounce cmd) $ \(e :: SomeException) ->
            publishError publish Nothing ("Command handler error: " <> T.pack (show e))

    -- Wait for shutdown signal
    waitShutdown

    -- Publish shutdown observation
    publishShuttingDown publish

-- | Publish Ready observation
publishReady :: ObservationPublisher -> Text -> Text -> UTCTime -> IPv4 -> IO ()
publishReady publish instId version startedAt ip =
  publish $ ObsReady AgentInfo
    { instanceId = instId
    , version = version
    , startedAt = startedAt
    , ipAddress = ip
    }

-- | Publish Output observation (stdout/stderr chunk)
--
-- Content should be base64 encoded before calling this.
publishOutput :: ObservationPublisher -> CommandId -> Channel -> Text -> IO ()
publishOutput publish cmdId channel content =
  publish $ ObsOutput OutputEvent
    { commandId = cmdId
    , channel = channel
    , content = content
    }

-- | Publish Output observation from raw bytes
-- Handles base64 encoding internally
publishOutputBytes :: ObservationPublisher -> CommandId -> Channel -> ByteString -> IO ()
publishOutputBytes publish cmdId channel bytes =
  publishOutput publish cmdId channel (TE.decodeUtf8 $ B64.encode bytes)

-- | Publish Complete observation
publishComplete :: ObservationPublisher -> CommandId -> Int -> Double -> IO ()
publishComplete publish cmdId exitCode duration =
  publish $ ObsComplete CompleteEvent
    { commandId = cmdId
    , exitCode = exitCode
    , duration = duration
    }

-- | Publish Error observation
publishError :: ObservationPublisher -> Maybe CommandId -> Text -> IO ()
publishError publish mCmdId message =
  publish $ ObsError ErrorEvent
    { commandId = mCmdId
    , message = message
    }

-- | Publish Metrics observation
publishMetrics :: ObservationPublisher -> MetricsEvent -> IO ()
publishMetrics publish event = publish $ ObsMetrics event

-- | Publish ShuttingDown observation
publishShuttingDown :: ObservationPublisher -> IO ()
publishShuttingDown publish = publish ObsShuttingDown

--------------------------------------------------------------------------------
-- Ready Announcement (for template builds)
--------------------------------------------------------------------------------

-- | Announcement message sent when agent is ready
--
-- Used during template builds to announce the agent's IP address
-- to the orchestrator via NATS instead of requiring ARP discovery.
data ReadyAnnouncement = ReadyAnnouncement
  { instanceId :: !Text   -- ^ Instance/template ID
  , ipAddress  :: !Text   -- ^ Agent's IP address (e.g., "10.99.0.89")
  , port       :: !Int    -- ^ Agent HTTP port (e.g., 8080)
  , ready      :: !Bool   -- ^ Always True when announced
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON ReadyAnnouncement where
  toJSON (ReadyAnnouncement instId ip p r) = object
    [ "instanceId" .= instId
    , "ip" .= ip
    , "port" .= p
    , "ready" .= r
    ]

instance FromJSON ReadyAnnouncement where
  parseJSON = withObject "ReadyAnnouncement" $ \o -> do
    instId <- o .: "instanceId"
    ip <- o .: "ip"
    p <- o .: "port"
    r <- o .: "ready"
    pure ReadyAnnouncement
      { instanceId = instId
      , ipAddress = ip
      , port = p
      , ready = r
      }

-- | Publish ready announcement to the specified subject
--
-- Called during template builds to announce the agent's IP to the orchestrator.
publishReadyAnnouncement
  :: NatsConnection
  -> Text               -- ^ Subject to publish to
  -> Text               -- ^ Instance ID
  -> Text               -- ^ IP address
  -> Int                -- ^ HTTP port
  -> IO ()
publishReadyAnnouncement conn subject instId ip httpPort = do
  let announcement = ReadyAnnouncement
        { instanceId = instId
        , ipAddress = ip
        , port = httpPort
        , ready = True
        }
  publishJson conn subject announcement

-- | Discover the agent's IP address
--
-- Uses network-info to find the first valid non-loopback IPv4 address.
-- Prefers common VM interface names (eth0, eth1, ens3, enp0s3) but falls
-- back to any interface with a private IP (10.x, 172.16-31.x, 192.168.x).
-- Falls back to loopback (127.0.0.1) if no other address is found.
discoverIpAddress :: IO IPv4
discoverIpAddress = do
  putStrLn "[Agent] Discovering IP address..."
  interfaces <- getNetworkInterfaces

  -- Log all interfaces for debugging
  putStrLn $ "[Agent] Found " ++ show (length interfaces) ++ " interfaces"
  mapM_ logInterface interfaces

  -- Find first valid IP on preferred interfaces
  let preferredNames = ["eth0", "eth1", "ens3", "enp0s3"]
      preferredIps =
        [ (name iface, ipv4 iface)
        | iface <- interfaces
        , name iface `elem` preferredNames
        , isValidIp (ipv4 iface)
        ]
      -- Fallback: any interface with a private IP
      fallbackIps =
        [ (name iface, ipv4 iface)
        | iface <- interfaces
        , name iface `notElem` ["lo"]  -- Exclude loopback
        , isPrivateIp (ipv4 iface)
        ]

  case preferredIps of
    ((ifaceName, ip):_) -> do
      let result = toNetIPv4 ip
      putStrLn $ "[Agent] Using IP " ++ T.unpack (IPv4.encode result) ++ " from " ++ ifaceName
      pure result
    [] -> case fallbackIps of
      ((ifaceName, ip):_) -> do
        let result = toNetIPv4 ip
        putStrLn $ "[Agent] Using fallback IP " ++ T.unpack (IPv4.encode result) ++ " from " ++ ifaceName
        pure result
      [] -> do
        putStrLn "[Agent] WARNING: No valid IP address found, using loopback"
        pure IPv4.loopback
  where
    logInterface :: NetworkInterface -> IO ()
    logInterface iface =
      putStrLn $ "[Agent]   " ++ name iface ++ ": " ++ T.unpack (IPv4.encode (toNetIPv4 (ipv4 iface)))

    isValidIp :: NI.IPv4 -> Bool
    isValidIp (NI.IPv4 addr) =
      addr /= 0 &&          -- Not 0.0.0.0
      addr /= 0x7f000001    -- Not 127.0.0.1

    -- Check if IP is in private ranges (10.x, 172.16-31.x, 192.168.x)
    isPrivateIp :: NI.IPv4 -> Bool
    isPrivateIp (NI.IPv4 addr) =
      let b0 = addr .&. 0xff
          b1 = (addr `div` 0x100) .&. 0xff
      in (b0 == 10) ||                           -- 10.0.0.0/8
         (b0 == 172 && b1 >= 16 && b1 <= 31) ||  -- 172.16.0.0/12
         (b0 == 192 && b1 == 168)                -- 192.168.0.0/16

    -- Convert Network.Info.IPv4 to Net.IPv4.IPv4
    -- Use show/decode to avoid byte order issues
    toNetIPv4 :: NI.IPv4 -> IPv4
    toNetIPv4 ip = case IPv4.decode (T.pack $ show ip) of
      Just v  -> v
      Nothing -> IPv4.loopback  -- Fallback (shouldn't happen)
