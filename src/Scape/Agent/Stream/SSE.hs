-- scape/src/Scape/Agent/Stream/SSE.hs
module Scape.Agent.Stream.SSE
  ( sseHandler
  , openAIHandler
  ) where

import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Servant (ServerError, err401, errBody)
import Servant.Types.SourceT (SourceT, source)

import Scape.Agent.State (AgentState)
import Scape.Protocol.Types (Token(..))
import Scape.Protocol.AISDK

-- | Handle Vercel AI SDK compatible chat endpoint
--
-- This endpoint accepts a ChatRequest and returns a stream of ChatEvents
-- in SSE format. For MVP, this echoes back the request
-- since the actual AI execution happens via the orchestrator.
--
-- Returns Either ServerError for auth failures, or the streaming source.
sseHandler
  :: AgentState
  -> Maybe Token
  -> ChatRequest
  -> IO (Either ServerError (SourceT IO ChatEvent))
sseHandler _state mToken req = do
  -- Validate token (simple check for now)
  case mToken of
    Nothing -> pure $ Left err401 { errBody = "Missing Authorization header" }
    Just _ -> do
      -- For MVP: echo back the last user message
      -- Real implementation would coordinate with orchestrator to run commands
      let lastUserMsg = findLastUserMessage (crMessages req)
          responseText = maybe "No message provided" cmContent lastUserMsg

      -- Return streaming source with events
      pure $ Right $ source
        [ ChatTextDelta responseText
        , ChatDone
        ]
  where
    findLastUserMessage :: [ChatMessage] -> Maybe ChatMessage
    findLastUserMessage = foldr findUser Nothing
      where
        findUser msg acc = case cmRole msg of
          User -> Just msg
          _ -> acc

-- | Handle OpenAI compatible endpoint
--
-- This endpoint accepts OpenAI-format requests and returns streaming
-- chunks in SSE format. For MVP, echoes back the request.
--
-- Returns Either ServerError for auth failures, or the streaming source.
openAIHandler
  :: AgentState
  -> Maybe Token
  -> OpenAIChatRequest
  -> IO (Either ServerError (SourceT IO OpenAIStreamChunk))
openAIHandler _state mToken req = do
  case mToken of
    Nothing -> pure $ Left err401 { errBody = "Missing Authorization header" }
    Just _ -> do
      -- Generate chunk ID
      chunkId <- T.pack . show <$> nextRandom

      -- For MVP: echo back the first message content
      -- Real impl would forward to orchestrator
      let content = maybe "" omContent $ listToMaybe $ ocrMessages req
          model = ocrModel req

      -- Return streaming source with chunks
      pure $ Right $ source
        [ -- Initial chunk with role
          OpenAIStreamChunk
            { oscId = chunkId
            , oscObject = "chat.completion.chunk"
            , oscCreated = 0
            , oscModel = model
            , oscChoices = [OpenAIChoice 0 (OpenAIDelta (Just "assistant") Nothing) Nothing]
            }
        , -- Content chunk
          OpenAIStreamChunk
            { oscId = chunkId
            , oscObject = "chat.completion.chunk"
            , oscCreated = 0
            , oscModel = model
            , oscChoices = [OpenAIChoice 0 (OpenAIDelta Nothing (Just content)) Nothing]
            }
        , -- Final chunk with finish reason
          OpenAIStreamChunk
            { oscId = chunkId
            , oscObject = "chat.completion.chunk"
            , oscCreated = 0
            , oscModel = model
            , oscChoices = [OpenAIChoice 0 (OpenAIDelta Nothing Nothing) (Just "stop")]
            }
        ]
  where
    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x
