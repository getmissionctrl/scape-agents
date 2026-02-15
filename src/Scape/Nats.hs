-- | NATS client wrapper for Scape
--
-- Provides connection management and type-safe pub/sub for
-- orchestrator-agent communication.
--
-- Uses natskell library underneath.
module Scape.Nats
  ( -- * Connection
    NatsConnection (..)
  , NatsConfig (..)
  , connectNats
  , disconnectNats
  , withNats

    -- * Publishing
  , publish
  , publishJson

    -- * Subscribing
  , subscribe
  , subscribeJson
  , SubscriptionId
  , unsubscribe

    -- * Request/Reply
  , request
  , requestJson

    -- * Re-exports
  , Message (..)
  ) where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Control.Concurrent.MVar

import qualified Client as Nats
import Client (MsgView(..), LoggerConfig(..), LogLevel(..), withLoggerConfig)
import Control.Concurrent.STM (newTMVarIO)

-- | NATS connection configuration
data NatsConfig = NatsConfig
  { url       :: !Text    -- ^ NATS server URL (e.g., "nats://localhost:4222")
  , user      :: !(Maybe Text)
  , password  :: !(Maybe Text)
  , credsFile :: !(Maybe FilePath)  -- ^ Path to .creds file for JWT auth
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Parsed message from NATS subscription
data Message = Message
  { msgSubject :: !ByteString
  , msgPayload :: !(Maybe ByteString)
  , msgReplyTo :: !(Maybe ByteString)
  }
  deriving stock (Show, Eq)

-- | Active NATS connection
newtype NatsConnection = NatsConnection
  { client :: Nats.Client
  }

-- | Subscription ID for unsubscribing
newtype SubscriptionId = SubscriptionId ByteString
  deriving stock (Eq, Show)

-- | Convert natskell MsgView to our Message type
msgViewToMessage :: MsgView -> Message
msgViewToMessage (MsgView subj _sid reply pay _hdrs) = Message
  { msgSubject = subj
  , msgPayload = pay
  , msgReplyTo = reply
  }

-- | Parse URL to extract host and port
-- Accepts "nats://host:port" or "host:port"
parseNatsUrl :: Text -> (String, Int)
parseNatsUrl urlText =
  let stripped = maybe urlText id $ T.stripPrefix "nats://" urlText
      (host, portPart) = T.breakOn ":" stripped
      port = case T.stripPrefix ":" portPart of
        Just p -> read (T.unpack p)
        Nothing -> 4222  -- default NATS port
  in (T.unpack host, port)

-- | Parsed credentials from a NATS creds file
data NatsCreds = NatsCreds
  { credsJwt  :: !ByteString
  , credsSeed :: !ByteString
  }

-- | Parse JWT and NKey seed from a NATS creds file
-- Creds file has JWT between "-----BEGIN NATS USER JWT-----" markers
-- and NKey seed between "-----BEGIN USER NKEY SEED-----" markers
parseCredsFile :: FilePath -> IO (Maybe NatsCreds)
parseCredsFile path = do
  contents <- readFile path
  let lns = lines contents
      extractBetween start end =
        case drop 1 $ dropWhile (/= start) lns of
          (val:_) | val /= end -> Just val
          _ -> Nothing
      mJwt = extractBetween "-----BEGIN NATS USER JWT-----" "------END NATS USER JWT------"
      mSeed = extractBetween "-----BEGIN USER NKEY SEED-----" "------END USER NKEY SEED------"
  pure $ case (mJwt, mSeed) of
    (Just jwt, Just seed) -> Just NatsCreds
      { credsJwt = TE.encodeUtf8 $ T.pack jwt
      , credsSeed = TE.encodeUtf8 $ T.pack seed
      }
    _ -> Nothing

-- | Connect to NATS server
connectNats :: NatsConfig -> IO NatsConnection
connectNats cfg = do
  let (host, port) = parseNatsUrl (url cfg)
      servers = [(host, port)]

  -- Create logger config with Info level (suppresses debug messages)
  logLock <- newTMVarIO ()
  let logCfg = LoggerConfig
        { minLogLevel = Info
        , logFn = \lvl msg -> putStrLn $ "[" ++ [firstChar lvl] ++ "] " ++ msg
        , logLock = logLock
        }
      firstChar Debug = 'D'
      firstChar Info  = 'I'
      firstChar Warn  = 'W'
      firstChar Error = 'E'
      firstChar Fatal = 'F'

  -- Build auth options based on config
  -- Note: For JWT+NKey auth, use withJWTCreds which properly combines them
  -- (withJWT and withNKey separately would overwrite each other)
  authOpts <- case credsFile cfg of
    Just path -> do
      mCreds <- parseCredsFile path
      case mCreds of
        Just creds -> pure [Nats.withJWTCreds (credsJwt creds) (credsSeed creds)]
        Nothing -> error $ "Failed to parse creds file: " <> path
    Nothing -> case (user cfg, password cfg) of
      (Just u, Just p) -> pure [Nats.withUserPass (TE.encodeUtf8 u, TE.encodeUtf8 p)]
      _ -> pure []

  c <- Nats.newClient servers (withLoggerConfig logCfg : authOpts)
  pure $ NatsConnection c

-- | Disconnect from NATS server
disconnectNats :: NatsConnection -> IO ()
disconnectNats (NatsConnection c) = Nats.close c

-- | Bracket pattern for NATS connections
withNats :: NatsConfig -> (NatsConnection -> IO a) -> IO a
withNats cfg = bracket (connectNats cfg) disconnectNats

-- | Publish raw bytes to a subject
publish :: NatsConnection -> Text -> ByteString -> IO ()
publish (NatsConnection c) subj pay =
  Nats.publish c (TE.encodeUtf8 subj) [Nats.withPayload pay]

-- | Publish JSON-encoded data to a subject
publishJson :: ToJSON a => NatsConnection -> Text -> a -> IO ()
publishJson conn subj a =
  publish conn subj (LBS.toStrict $ encode a)

-- | Subscribe to a subject pattern with callback
subscribe :: NatsConnection -> Text -> (Message -> IO ()) -> IO SubscriptionId
subscribe (NatsConnection c) subj callback = do
  sid <- Nats.subscribe c (TE.encodeUtf8 subj) $ \msgView ->
    callback (msgViewToMessage msgView)
  pure $ SubscriptionId sid

-- | Subscribe to a subject and decode JSON messages
subscribeJson :: FromJSON a => NatsConnection -> Text -> (Either String a -> IO ()) -> IO SubscriptionId
subscribeJson conn subj callback =
  subscribe conn subj $ \msg ->
    case msgPayload msg of
      Nothing -> callback (Left "Empty message payload")
      Just pay -> callback (eitherDecode $ LBS.fromStrict pay)

-- | Unsubscribe from a subject
unsubscribe :: NatsConnection -> SubscriptionId -> IO ()
unsubscribe (NatsConnection c) (SubscriptionId sid) = Nats.unsubscribe c sid

-- | Request/reply pattern (synchronous)
-- Sends a request and waits for a reply with timeout
request :: NatsConnection -> Text -> ByteString -> Int -> IO (Maybe Message)
request (NatsConnection c) subj pay _timeoutMs = do
  -- natskell uses callback-based request/reply
  -- We need to bridge to synchronous via MVar
  resultVar <- newEmptyMVar
  Nats.publish c (TE.encodeUtf8 subj)
    [ Nats.withPayload pay
    , Nats.withReplyCallback $ \msgView -> do
        putMVar resultVar (msgViewToMessage msgView)
    ]
  -- TODO: Add timeout handling
  msg <- takeMVar resultVar
  pure $ Just msg

-- | Request/reply with JSON encoding
requestJson :: (ToJSON a, FromJSON b) => NatsConnection -> Text -> a -> Int -> IO (Either String b)
requestJson conn subj a timeoutMs = do
  mResp <- request conn subj (LBS.toStrict $ encode a) timeoutMs
  pure $ case mResp of
    Nothing -> Left "Request timed out"
    Just msg -> case msgPayload msg of
      Nothing -> Left "Empty response payload"
      Just pay -> eitherDecode (LBS.fromStrict pay)
