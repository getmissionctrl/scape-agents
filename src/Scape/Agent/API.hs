-- scape/src/Scape/Agent/API.hs
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Scape.Agent.API
  ( AgentAPI
  , agentAPI
  , HealthStatus (..)
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.Types.SourceT (SourceT)

import Scape.Protocol.Types (CommandId, Token)
import Scape.Protocol.AISDK (ChatRequest, ChatEvent, OpenAIChatRequest, OpenAIStreamChunk)

-- | Health check response
data HealthStatus = HealthStatus
  { hsStatus  :: !Text
  , hsUptime  :: !Double
  , hsVersion :: !Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | Complete Agent API
type AgentAPI =
       -- Health & metrics
       "healthz" :> Get '[JSON] HealthStatus
  :<|> "metrics" :> Get '[PlainText] Text

       -- WebSocket streaming (interactive/terminal)
  :<|> "stream" :> Capture "commandId" CommandId
       :> QueryParam "token" Token
       :> WebSocket

       -- Vercel AI SDK compatible chat endpoint (NDJSON streaming)
  :<|> "api" :> "chat"
       :> Header "Authorization" Token
       :> ReqBody '[JSON] ChatRequest
       :> StreamPost NewlineFraming JSON (SourceT IO ChatEvent)

       -- OpenAI compatible endpoint (NDJSON streaming)
  :<|> "v1" :> "chat" :> "completions"
       :> Header "Authorization" Token
       :> ReqBody '[JSON] OpenAIChatRequest
       :> StreamPost NewlineFraming JSON (SourceT IO OpenAIStreamChunk)

agentAPI :: Proxy AgentAPI
agentAPI = Proxy
