-- scape/src/Scape/Agent/State.hs
module Scape.Agent.State
  ( AgentState (..)
  , CommandState (..)
  , Subscriber (..)
  , AgentMetrics (..)
  , ControlEventInternal (..)
  , initAgentState
  , injectSecret
  , getSecret
  , getAllSecrets
  , clearSecrets
  , registerCommand
  , unregisterCommand
  , getCommand
  , addSubscriber
  , removeSubscriber
  , getSubscribers
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Optics.Core ((%~), (&))
import System.Process (ProcessHandle)

import Scape.Protocol.Types (CommandId, Token)
import Scape.Protocol.Control (OutputChannel)
import Scape.Protocol.AISDK (ChatEvent)

-- | Shared agent state (STM-based for thread safety)
data AgentState = AgentState
  { secrets     :: !(TVar (Map Text Text))
  , commands    :: !(TVar (Map CommandId CommandState))
  , subscribers :: !(TVar (Map CommandId [Subscriber]))
  , metrics     :: !(TVar AgentMetrics)
  , eventQueue  :: !(TQueue ControlEventInternal)
  }

-- | State for an active command
data CommandState = CommandState
  { processHandle :: !ProcessHandle
  , startedAt     :: !UTCTime
  , timeout       :: !Int
  , stdinChan     :: !(TChan ByteString)
  , token         :: !Token
  }

-- | Stream subscriber (WS or SSE client)
data Subscriber
  = WSSubscriber !(TChan ByteString)
  | SSESubscriber !(TChan ChatEvent)

-- | Internal metrics (not exported to protocol to avoid cycles)
data AgentMetrics = AgentMetrics
  { commandsTotal  :: !Int
  , commandsActive :: !Int
  , bytesStreamed  :: !Int
  , errorsTotal    :: !Int
  , wsConns        :: !Int
  , sseConns       :: !Int
  , cpuPercent     :: !Double
  , memUsedKb      :: !Int
  , memTotalKb     :: !Int
  , netRxBytes     :: !Int64
  , netTxBytes     :: !Int64
  , diskUsedKb     :: !Int
  , diskTotalKb    :: !Int
  }
  deriving stock (Generic)

-- | Internal event type (to break import cycle)
data ControlEventInternal
  = CEStreamReady !CommandId !Int !Token
  | CECommandOutput !CommandId !OutputChannel !Text  -- ^ Output chunk (text, not base64)
  | CECommandComplete !CommandId !Int !Double
  | CECommandError !CommandId !Text
  deriving (Show)

-- | Initialize empty agent state
initAgentState :: IO AgentState
initAgentState = AgentState
  <$> newTVarIO Map.empty
  <*> newTVarIO Map.empty
  <*> newTVarIO Map.empty
  <*> newTVarIO (AgentMetrics 0 0 0 0 0 0 0.0 0 0 0 0 0 0)
  <*> newTQueueIO

-- | Inject a secret into session scope
injectSecret :: AgentState -> Text -> Text -> IO ()
injectSecret state key value = atomically $
  modifyTVar' (secrets state) (Map.insert key value)

-- | Get a secret by key
getSecret :: AgentState -> Text -> IO (Maybe Text)
getSecret state key = Map.lookup key <$> readTVarIO (secrets state)

-- | Get all secrets (for command execution)
getAllSecrets :: AgentState -> IO (Map Text Text)
getAllSecrets state = readTVarIO (secrets state)

-- | Clear all secrets
clearSecrets :: AgentState -> IO ()
clearSecrets state = atomically $ writeTVar (secrets state) Map.empty

-- | Register an active command
registerCommand :: AgentState -> CommandId -> CommandState -> IO ()
registerCommand state cmdId cmdState = atomically $ do
  modifyTVar' (commands state) (Map.insert cmdId cmdState)
  modifyTVar' (metrics state) $ \m -> m
    & #commandsTotal %~ (+ 1)
    & #commandsActive %~ (+ 1)

-- | Unregister a completed command
unregisterCommand :: AgentState -> CommandId -> IO ()
unregisterCommand state cmdId = atomically $ do
  modifyTVar' (commands state) (Map.delete cmdId)
  modifyTVar' (subscribers state) (Map.delete cmdId)
  modifyTVar' (metrics state) $ \m -> m
    & #commandsActive %~ (\n -> max 0 (n - 1))

-- | Get command state
getCommand :: AgentState -> CommandId -> IO (Maybe CommandState)
getCommand state cmdId = Map.lookup cmdId <$> readTVarIO (commands state)

-- | Add a stream subscriber
addSubscriber :: AgentState -> CommandId -> Subscriber -> IO ()
addSubscriber state cmdId sub = atomically $ do
  modifyTVar' (subscribers state) $
    Map.insertWith (++) cmdId [sub]
  modifyTVar' (metrics state) $ \m -> case sub of
    WSSubscriber _ -> m & #wsConns %~ (+ 1)
    SSESubscriber _ -> m & #sseConns %~ (+ 1)

-- | Remove a subscriber (on disconnect)
removeSubscriber :: AgentState -> CommandId -> Subscriber -> IO ()
removeSubscriber state cmdId sub = atomically $ do
  modifyTVar' (subscribers state) $
    Map.adjust (filter (/= sub)) cmdId
  modifyTVar' (metrics state) $ \m -> case sub of
    WSSubscriber _ -> m & #wsConns %~ (\n -> max 0 (n - 1))
    SSESubscriber _ -> m & #sseConns %~ (\n -> max 0 (n - 1))

-- Need Eq instance for Subscriber filtering
instance Eq Subscriber where
  -- Note: TChan doesn't have Eq, so we can't properly compare
  -- In practice, we'd use a unique ID per subscriber
  _ == _ = False

-- | Get all subscribers for a command
getSubscribers :: AgentState -> CommandId -> IO [Subscriber]
getSubscribers state cmdId =
  Map.findWithDefault [] cmdId <$> readTVarIO (subscribers state)
