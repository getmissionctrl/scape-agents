-- scape/src/Scape/Agent/Server.hs
module Scape.Agent.Server
  ( runServer
  , ServerConfig (..)
  , defaultServerConfig
  ) where

import Control.Concurrent.STM (readTVarIO)
import Optics.Core ((^.))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Katip
import Network.Wai.Handler.Warp (run)
import qualified Network.WebSockets as WS
import Servant
import Servant.Types.SourceT (SourceT)

import Scape.Agent.API
import Scape.Agent.Logging
import Scape.Agent.State
import Scape.Agent.Stream.WebSocket (wsHandler)
import Scape.Agent.Stream.SSE (sseHandler, openAIHandler)
import Scape.Agent.Terminal (terminalHandler)
import Scape.Protocol.Types (CommandId, Token)
import Scape.Protocol.AISDK (ChatRequest, ChatEvent, OpenAIChatRequest, OpenAIStreamChunk)

-- | Server configuration
data ServerConfig = ServerConfig
  { scPort    :: !Int       -- ^ HTTP server port
  , scVersion :: !Text      -- ^ Agent version string
  }

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { scPort = 8080
  , scVersion = "0.1.0"
  }

-- | Run the HTTP/WS/SSE server
runServer :: AgentEnv -> UTCTime -> ServerConfig -> IO ()
runServer env startTime config = do
  logInfoIO (aeLogEnv env) "server" $
    ls ("Agent server starting on port " <> T.pack (show (scPort config)))
  run (scPort config) $ serve agentAPI (hoistServer agentAPI (appToHandler env) (server startTime config))

-- | Natural transformation from AppM to Handler
appToHandler :: AgentEnv -> AppM a -> Handler a
appToHandler env action = liftIO $ runAppM env action

-- | Server implementation in AppM
server :: UTCTime -> ServerConfig -> ServerT AgentAPI AppM
server startTime config =
       healthHandler startTime config
  :<|> metricsHandler
  :<|> wsEndpoint
  :<|> sseEndpoint
  :<|> openAIEndpoint
  :<|> terminalEndpoint

-- | Health check endpoint
healthHandler :: UTCTime -> ServerConfig -> AppM HealthStatus
healthHandler startTime config = do
  now <- liftIO getCurrentTime
  let uptime = realToFrac $ diffUTCTime now startTime
  $(logTM) DebugS "Health check requested"
  pure HealthStatus
    { hsStatus = "ok"
    , hsUptime = uptime
    , hsVersion = scVersion config
    }

-- | Prometheus metrics endpoint
metricsHandler :: AppM Text
metricsHandler = do
  state <- asks aeState
  m <- liftIO $ readTVarIO (metrics state)
  $(logTM) DebugS "Metrics requested"
  pure $ T.unlines
    [ "# HELP scape_agent_commands_total Total commands executed"
    , "# TYPE scape_agent_commands_total counter"
    , "scape_agent_commands_total " <> T.pack (show $ m ^. #commandsTotal)
    , ""
    , "# HELP scape_agent_commands_active Currently running commands"
    , "# TYPE scape_agent_commands_active gauge"
    , "scape_agent_commands_active " <> T.pack (show $ m ^. #commandsActive)
    , ""
    , "# HELP scape_agent_bytes_streamed Total bytes streamed"
    , "# TYPE scape_agent_bytes_streamed counter"
    , "scape_agent_bytes_streamed " <> T.pack (show $ m ^. #bytesStreamed)
    , ""
    , "# HELP scape_agent_errors_total Total errors"
    , "# TYPE scape_agent_errors_total counter"
    , "scape_agent_errors_total " <> T.pack (show $ m ^. #errorsTotal)
    , ""
    , "# HELP scape_agent_ws_connections Active WebSocket connections"
    , "# TYPE scape_agent_ws_connections gauge"
    , "scape_agent_ws_connections " <> T.pack (show $ m ^. #wsConns)
    , ""
    , "# HELP scape_agent_sse_connections Active SSE connections"
    , "# TYPE scape_agent_sse_connections gauge"
    , "scape_agent_sse_connections " <> T.pack (show $ m ^. #sseConns)
    , ""
    , "# HELP scape_agent_cpu_percent CPU usage percentage"
    , "# TYPE scape_agent_cpu_percent gauge"
    , "scape_agent_cpu_percent " <> T.pack (show $ m ^. #cpuPercent)
    , ""
    , "# HELP scape_agent_memory_used_kb Memory used in KB"
    , "# TYPE scape_agent_memory_used_kb gauge"
    , "scape_agent_memory_used_kb " <> T.pack (show $ m ^. #memUsedKb)
    , ""
    , "# HELP scape_agent_memory_total_kb Total memory in KB"
    , "# TYPE scape_agent_memory_total_kb gauge"
    , "scape_agent_memory_total_kb " <> T.pack (show $ m ^. #memTotalKb)
    , ""
    , "# HELP scape_agent_net_rx_bytes Network bytes received"
    , "# TYPE scape_agent_net_rx_bytes counter"
    , "scape_agent_net_rx_bytes " <> T.pack (show $ m ^. #netRxBytes)
    , ""
    , "# HELP scape_agent_net_tx_bytes Network bytes transmitted"
    , "# TYPE scape_agent_net_tx_bytes counter"
    , "scape_agent_net_tx_bytes " <> T.pack (show $ m ^. #netTxBytes)
    , ""
    , "# HELP scape_agent_disk_used_kb Disk used in KB"
    , "# TYPE scape_agent_disk_used_kb gauge"
    , "scape_agent_disk_used_kb " <> T.pack (show $ m ^. #diskUsedKb)
    , ""
    , "# HELP scape_agent_disk_total_kb Total disk in KB"
    , "# TYPE scape_agent_disk_total_kb gauge"
    , "scape_agent_disk_total_kb " <> T.pack (show $ m ^. #diskTotalKb)
    ]

-- | WebSocket endpoint handler
-- Note: WebSocket handlers run in IO, not AppM, so we extract state
wsEndpoint :: CommandId -> Maybe Token -> WS.Connection -> AppM ()
wsEndpoint cmdId mToken conn = do
  state <- asks aeState
  $(logTM) InfoS $ ls ("WebSocket connection for command " <> show cmdId)
  liftIO $ wsHandler state cmdId mToken conn

-- | SSE chat endpoint (delegates to SSE module)
sseEndpoint :: Maybe Token -> ChatRequest -> AppM (SourceT IO ChatEvent)
sseEndpoint mToken req = do
  state <- asks aeState
  $(logTM) InfoS "Chat SSE request"
  result <- liftIO $ sseHandler state mToken req
  either throwAppError pure result

-- | OpenAI endpoint (delegates to SSE module)
openAIEndpoint :: Maybe Token -> OpenAIChatRequest -> AppM (SourceT IO OpenAIStreamChunk)
openAIEndpoint mToken req = do
  state <- asks aeState
  $(logTM) InfoS "OpenAI API request"
  result <- liftIO $ openAIHandler state mToken req
  either throwAppError pure result

-- | Terminal WebSocket endpoint (PTY over WebSocket)
terminalEndpoint :: WS.Connection -> AppM ()
terminalEndpoint conn = do
  $(logTM) InfoS "Terminal WebSocket connection"
  liftIO $ terminalHandler conn

-- | Throw a ServerError in AppM (via IO exception)
-- Since AppM is ReaderT over IO, we throw ServerError as an exception
-- and the appToHandler will catch it and convert to Handler error
throwAppError :: ServerError -> AppM a
throwAppError = liftIO . throwIO
