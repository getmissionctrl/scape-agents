-- scape/src/Scape/Protocol/AISDK.hs
module Scape.Protocol.AISDK
  ( -- * Chat types (Vercel AI SDK compatible)
    ChatRequest (..)
  , ChatMessage (..)
  , ChatRole (..)
  , ChatEvent (..)

    -- * OpenAI compatible types
  , OpenAIChatRequest (..)
  , OpenAIMessage (..)
  , OpenAIStreamChunk (..)
  , OpenAIDelta (..)
  , OpenAIChoice (..)

    -- * Completion types
  , CompletionRequest (..)
  , CompletionEvent (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Chat role
data ChatRole = User | Assistant | System
  deriving stock (Eq, Show, Generic)

instance ToJSON ChatRole where
  toJSON User = "user"
  toJSON Assistant = "assistant"
  toJSON System = "system"

instance FromJSON ChatRole where
  parseJSON = withText "ChatRole" $ \case
    "user" -> pure User
    "assistant" -> pure Assistant
    "system" -> pure System
    other -> fail $ "Unknown role: " <> show other

-- | Chat message
data ChatMessage = ChatMessage
  { cmRole    :: !ChatRole
  , cmContent :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Vercel AI SDK chat request
data ChatRequest = ChatRequest
  { crMessages    :: ![ChatMessage]
  , crModel       :: !(Maybe Text)
  , crTemperature :: !(Maybe Double)
  , crMaxTokens   :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | SSE events for chat streaming (Vercel AI SDK format)
data ChatEvent
  = ChatTextDelta !Text        -- ^ Text chunk
  | ChatToolCall !Text !Value  -- ^ Tool name, arguments
  | ChatToolResult !Text !Value -- ^ Tool name, result
  | ChatError !Text            -- ^ Error message
  | ChatDone                   -- ^ Stream complete
  deriving stock (Eq, Show, Generic)

instance ToJSON ChatEvent where
  toJSON (ChatTextDelta t) = object ["type" .= ("text" :: Text), "content" .= t]
  toJSON (ChatToolCall n a) = object ["type" .= ("tool_call" :: Text), "name" .= n, "arguments" .= a]
  toJSON (ChatToolResult n r) = object ["type" .= ("tool_result" :: Text), "name" .= n, "result" .= r]
  toJSON (ChatError e) = object ["type" .= ("error" :: Text), "message" .= e]
  toJSON ChatDone = object ["type" .= ("done" :: Text)]

-- | OpenAI-compatible chat request
data OpenAIChatRequest = OpenAIChatRequest
  { ocrModel       :: !Text
  , ocrMessages    :: ![OpenAIMessage]
  , ocrStream      :: !(Maybe Bool)
  , ocrTemperature :: !(Maybe Double)
  , ocrMaxTokens   :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON OpenAIChatRequest where
  parseJSON = withObject "OpenAIChatRequest" $ \o -> OpenAIChatRequest
    <$> o .: "model"
    <*> o .: "messages"
    <*> o .:? "stream"
    <*> o .:? "temperature"
    <*> o .:? "max_tokens"

instance ToJSON OpenAIChatRequest where
  toJSON OpenAIChatRequest{..} = object
    [ "model" .= ocrModel
    , "messages" .= ocrMessages
    , "stream" .= ocrStream
    , "temperature" .= ocrTemperature
    , "max_tokens" .= ocrMaxTokens
    ]

data OpenAIMessage = OpenAIMessage
  { omRole    :: !Text
  , omContent :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | OpenAI streaming chunk
data OpenAIStreamChunk = OpenAIStreamChunk
  { oscId      :: !Text
  , oscObject  :: !Text
  , oscCreated :: !Int
  , oscModel   :: !Text
  , oscChoices :: ![OpenAIChoice]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OpenAIStreamChunk where
  toJSON OpenAIStreamChunk{..} = object
    [ "id" .= oscId
    , "object" .= oscObject
    , "created" .= oscCreated
    , "model" .= oscModel
    , "choices" .= oscChoices
    ]

data OpenAIChoice = OpenAIChoice
  { ocIndex        :: !Int
  , ocDelta        :: !OpenAIDelta
  , ocFinishReason :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OpenAIChoice where
  toJSON OpenAIChoice{..} = object
    [ "index" .= ocIndex
    , "delta" .= ocDelta
    , "finish_reason" .= ocFinishReason
    ]

data OpenAIDelta = OpenAIDelta
  { odRole    :: !(Maybe Text)
  , odContent :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OpenAIDelta where
  toJSON OpenAIDelta{..} = object
    [ "role" .= odRole
    , "content" .= odContent
    ]

-- | Simple completion request
data CompletionRequest = CompletionRequest
  { cprPrompt      :: !Text
  , cprMaxTokens   :: !(Maybe Int)
  , cprTemperature :: !(Maybe Double)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Completion streaming event
data CompletionEvent
  = CompletionDelta !Text
  | CompletionDone
  | CompletionError !Text
  deriving stock (Eq, Show, Generic)

instance ToJSON CompletionEvent where
  toJSON (CompletionDelta t) = object ["type" .= ("delta" :: Text), "text" .= t]
  toJSON CompletionDone = object ["type" .= ("done" :: Text)]
  toJSON (CompletionError e) = object ["type" .= ("error" :: Text), "message" .= e]
