-- scape/src/Scape/Agent/Vsock.hs
--
-- Client for communication with orchestrator.
-- Uses JSON over TCP (Unix socket for dev, vsock for production).
module Scape.Agent.Vsock
  ( connectToOrchestrator
  , VsockConfig (..)
  , ConnectionMode (..)
  , defaultVsockConfig
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Concurrent.STM
import Control.Exception (bracket, catch, SomeException)
import Control.Monad (forever)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Katip (LogEnv, LogStr, Namespace(..), logStr)
import Network.Socket

import Scape.Agent.Logging (logInfoIO, logWarnIO, logDebugIO)
import qualified Scape.Protocol.Transport as Proto
import Scape.Agent.MMDS (fetchMMDSConfig, MMDSConfig(..))
import Optics.Core ((^.))
import Scape.Agent.State (AgentState(..), injectSecret, ControlEventInternal(..), AgentMetrics(..))
import Scape.Agent.Executor (executeCommandAsync, cancelCommand)
import Scape.Protocol.Control
import Scape.Protocol.Types

-- | Helper to create LogStr from Text
logT :: Text -> LogStr
logT = logStr

-- | Connection mode for orchestrator communication
data ConnectionMode
  = ModeUnix   -- ^ Unix domain socket (development on host)
  | ModeVsock  -- ^ Vsock (production, not yet implemented - see docs/plans/2025-01-20-vsock-ffi-implementation.md)
  | ModeTcp    -- ^ TCP over network (Phase 0 VM testing)
  deriving (Show, Eq)

-- | Configuration for vsock/socket connection
data VsockConfig = VsockConfig
  { vcMode       :: !ConnectionMode
  , vcSocketPath :: !FilePath      -- ^ Unix socket path (dev mode)
  , vcVsockCID   :: !Word32        -- ^ Host CID (always 2 for host)
  , vcVsockPort  :: !Word32        -- ^ Vsock port
  , vcTcpHost    :: !String        -- ^ TCP host (Phase 0 VM testing)
  , vcTcpPort    :: !Int           -- ^ TCP port (Phase 0 VM testing)
  , vcRetryMs    :: !Int           -- ^ Retry interval on disconnect (ms)
  , vcInstanceId :: !(Maybe Text)  -- ^ Instance ID from MMDS (for handshake)
  }

-- | Default config for development (Unix socket)
defaultVsockConfig :: VsockConfig
defaultVsockConfig = VsockConfig
  { vcMode = ModeUnix
  , vcSocketPath = "/tmp/scape-orchestrator.sock"
  , vcVsockCID = 2         -- Host CID
  , vcVsockPort = 5000
  , vcTcpHost = "10.99.0.1"  -- Default: host IP on TAP network
  , vcTcpPort = 5000
  , vcRetryMs = 1000
  , vcInstanceId = Nothing
  }

-- | Placeholder for vsock socket creation
-- TODO (Phase 1): Implement via FFI - AF_VSOCK = 40 on Linux
vsockSocket :: IO Socket
vsockSocket = error "vsock not yet implemented - use Unix socket mode for now"

-- | Placeholder for vsock connect
-- TODO (Phase 1): Implement via FFI with sockaddr_vm struct
vsockConnect :: Socket -> Word32 -> Word32 -> IO ()
vsockConnect _sock _cid _port =
  error "vsock not yet implemented - use Unix socket mode for now"

-- | Connect to orchestrator and handle commands
--
-- This runs in a loop, reconnecting on failure.
-- For now uses Unix sockets; in production would use AF_VSOCK.
connectToOrchestrator :: AgentState -> LogEnv -> VsockConfig -> IO ()
connectToOrchestrator state logEnv config = forever $ do
  catch tryConnect $ \(e :: SomeException) -> do
    logWarnIO logEnv ns $ logT $ "Vsock connection failed: " <> T.pack (show e)
    logDebugIO logEnv ns $ logT $ "Retrying in " <> T.pack (show (vcRetryMs config)) <> "ms"
    threadDelay (vcRetryMs config * 1000)
  where
    ns = Namespace ["vsock"]

    tryConnect = case vcMode config of
      ModeUnix -> do
        let addr = SockAddrUnix (vcSocketPath config)
        logInfoIO logEnv ns $ logT $ "Connecting to orchestrator at " <> T.pack (vcSocketPath config)
        bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
          connect sock addr
          logInfoIO logEnv ns $ logT "Connected to orchestrator (Unix socket)"
          runConnection sock

      ModeVsock -> do
        logInfoIO logEnv ns $ logT $ "Connecting to orchestrator via vsock CID "
          <> T.pack (show $ vcVsockCID config) <> " port " <> T.pack (show $ vcVsockPort config)
        bracket vsockSocket close $ \sock -> do
          vsockConnect sock (vcVsockCID config) (vcVsockPort config)
          logInfoIO logEnv ns $ logT "Connected to orchestrator (vsock)"
          runConnection sock

      ModeTcp -> do
        let host = vcTcpHost config
            port = show (vcTcpPort config)
        logInfoIO logEnv ns $ logT $ "Connecting to orchestrator at " <> T.pack host <> ":" <> T.pack port
        -- Resolve address and connect
        addrInfos <- getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just port)
        case addrInfos of
          [] -> fail $ "Cannot resolve host: " <> host
          (addrInfo:_) -> bracket (openSocket addrInfo) close $ \sock -> do
            connect sock (addrAddress addrInfo)
            logInfoIO logEnv ns $ logT "Connected to orchestrator (TCP)"
            runConnection sock

    runConnection sock = do
      -- Send agent ready event with instanceId if available
      case vcInstanceId config of
        Just instId -> do
          logInfoIO logEnv ns $ logT $ "Sending AgentReady with instanceId: " <> instId
          Proto.sendEvent sock (EvAgentReady instId)
        Nothing ->
          logDebugIO logEnv ns $ logT "No instanceId available, skipping AgentReady"

      -- Start event sender thread
      eventSender <- async $ sendEvents state logEnv sock
      link eventSender
      -- Handle incoming commands
      handleCommands state logEnv sock

-- | Handle incoming commands from orchestrator
handleCommands :: AgentState -> LogEnv -> Socket -> IO ()
handleCommands state logEnv sock = forever $ do
  result <- Proto.recvMessage sock
  case result of
    Nothing -> do
      logWarnIO logEnv ns $ logT "Connection closed by orchestrator"
      fail "Connection closed"
    Just cmd -> handleCommand state logEnv sock cmd
  where
    ns = Namespace ["vsock", "recv"]

-- | Handle a single command from orchestrator
handleCommand :: AgentState -> LogEnv -> Socket -> ControlCommand -> IO ()
handleCommand state logEnv sock cmd = do
  logDebugIO logEnv ns $ logT $ "Received command: " <> T.pack (cmdName cmd)
  case cmd of
    CmdPing -> do
      logDebugIO logEnv ns $ logT "Responding to ping"
      sendEvent sock EvPong

    CmdRunCommand args -> do
      logInfoIO logEnv ns $ logT $ "Running command: " <> rcaCommand args
      -- Start async execution
      executeCommandAsync state
        (rcaCommandId args)
        (rcaCommand args)
        (rcaArgs args)
        (Map.toList $ rcaEnv args <> rcaSecrets args)
        (rcaWorkdir args)
        (rcaTimeout args)
        (Token "")  -- Token will be set by PrepareStream
      sendEvent sock $ EvCommandStarted (rcaCommandId args)

    CmdCancelCommand cmdId -> do
      logInfoIO logEnv ns $ logT $ "Cancelling command: " <> T.pack (show cmdId)
      cancelCommand state cmdId

    CmdInjectSecrets args -> do
      logDebugIO logEnv ns $ logT $ "Injecting secrets (scope: " <> T.pack (show $ isaScope args) <> ")"
      case isaScope args of
        ScopeSession ->
          mapM_ (uncurry $ injectSecret state) (Map.toList $ isaSecrets args)
        ScopeFile path -> do
          -- Write each secret to a file
          mapM_ (\(k, v) -> writeFile (path <> "/" <> show k) (show v))
                (Map.toList $ isaSecrets args)
        ScopeNextCommand ->
          -- Handled at command execution time
          pure ()

    CmdPrepareStream cmdId token -> do
      logDebugIO logEnv ns $ logT $ "Preparing stream for command: " <> T.pack (show cmdId)
      -- The token is used for WebSocket/SSE authentication
      -- For now just send ready event
      sendEvent sock $ EvStreamReady cmdId 8080 token

    CmdGetMetrics -> do
      logDebugIO logEnv ns $ logT "Sending metrics"
      m <- readTVarIO (metrics state)
      sendEvent sock $ EvMetrics $ AgentMetricsData
        { amdCommandsTotal = m ^. #commandsTotal
        , amdCommandsActive = m ^. #commandsActive
        , amdBytesStreamed = m ^. #bytesStreamed
        , amdErrorsTotal = m ^. #errorsTotal
        , amdWsConnections = m ^. #wsConns
        , amdSseConnections = m ^. #sseConns
        }

    CmdRefreshConfig -> do
      logInfoIO logEnv ns $ logT "Re-fetching MMDS configuration..."
      result <- fetchMMDSConfig logEnv 2000
      case result of
        Just config -> do
          logInfoIO logEnv ns $ logT $ "MMDS refreshed: instance-id=" <> instanceId config
          sendEvent sock $ EvConfigRefreshed (Just $ instanceId config)
        Nothing -> do
          logWarnIO logEnv ns $ logT "MMDS refresh failed - service unavailable"
          sendEvent sock $ EvConfigRefreshed Nothing

    CmdShutdown -> do
      logWarnIO logEnv ns $ logT "Shutdown requested by orchestrator"
      sendEvent sock EvShuttingDown
      -- In real impl would trigger graceful shutdown
      fail "Shutdown requested"

    -- File operations (placeholder implementations)
    CmdWriteFile _args -> pure ()
    CmdReadFile _path -> pure ()
    CmdListDir _path -> pure ()
    CmdDeletePath _path _recursive -> pure ()
  where
    ns = Namespace ["vsock", "cmd"]

    cmdName :: ControlCommand -> String
    cmdName = \case
      CmdPing -> "Ping"
      CmdRunCommand _ -> "RunCommand"
      CmdCancelCommand _ -> "CancelCommand"
      CmdInjectSecrets _ -> "InjectSecrets"
      CmdPrepareStream _ _ -> "PrepareStream"
      CmdGetMetrics -> "GetMetrics"
      CmdRefreshConfig -> "RefreshConfig"
      CmdShutdown -> "Shutdown"
      CmdWriteFile _ -> "WriteFile"
      CmdReadFile _ -> "ReadFile"
      CmdListDir _ -> "ListDir"
      CmdDeletePath _ _ -> "DeletePath"

-- | Send queued events back to orchestrator
sendEvents :: AgentState -> LogEnv -> Socket -> IO ()
sendEvents state logEnv sock = forever $ do
  event <- atomically $ readTQueue (eventQueue state)
  -- Only log non-output events to avoid spam
  case event of
    CECommandOutput {} -> pure ()
    _ -> logDebugIO logEnv ns $ logT $ "Sending event: " <> T.pack (eventName event)
  let controlEvent = case event of
        CEStreamReady cmdId port token -> EvStreamReady cmdId port token
        CECommandOutput cmdId channel text -> EvCommandOutput cmdId channel text
        CECommandComplete cmdId code dur -> EvCommandComplete cmdId (ExitCode code) dur
        CECommandError cmdId msg -> EvCommandError cmdId msg
  sendEvent sock controlEvent
  where
    ns = Namespace ["vsock", "send"]

    eventName :: ControlEventInternal -> String
    eventName = \case
      CEStreamReady {} -> "StreamReady"
      CECommandOutput {} -> "CommandOutput"
      CECommandComplete {} -> "CommandComplete"
      CECommandError {} -> "CommandError"

-- | Send a single event as length-prefixed JSON
sendEvent :: Socket -> ControlEvent -> IO ()
sendEvent = Proto.sendEvent
