-- scape/src/Scape/Agent/Logging.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Scape.Agent.Logging
  ( -- * Application environment
    AgentEnv (..)
  , initAgentEnv
  , closeAgentEnv

    -- * Application monad
  , AppM (..)
  , runAppM

    -- * Logging utilities
  , logIO
  , logDebugIO
  , logInfoIO
  , logWarnIO
  , logErrorIO

    -- * Re-exports from Katip
  , LogEnv
  , Namespace (..)
  , Severity (..)
  , katipAddNamespace
  , katipAddContext
  , sl
  , ls
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, asks, local, runReaderT)
import Katip
import System.IO (stdout)

import Scape.Agent.State (AgentState, initAgentState)

-- | Application environment containing state and logging
data AgentEnv = AgentEnv
  { aeState     :: !AgentState
  , aeLogEnv    :: !LogEnv
  , aeNamespace :: !Namespace
  , aeContext   :: !LogContexts
  }

-- | Initialize the agent environment with logging
initAgentEnv :: Namespace -> Severity -> IO AgentEnv
initAgentEnv ns minSeverity = do
  -- Create log environment
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem minSeverity) V2
  let mkLogEnv' = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv ns "scape-agent"
  logEnv <- mkLogEnv'

  -- Initialize agent state
  state <- initAgentState

  pure AgentEnv
    { aeState = state
    , aeLogEnv = logEnv
    , aeNamespace = ns
    , aeContext = mempty
    }

-- | Clean up logging resources
closeAgentEnv :: AgentEnv -> IO ()
closeAgentEnv env = void $ closeScribes (aeLogEnv env)

-- | Application monad with logging and state access
newtype AppM a = AppM { unAppM :: ReaderT AgentEnv IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AgentEnv
    )

instance MonadUnliftIO AppM where
  withRunInIO inner = AppM $ withRunInIO $ \run -> inner (run . unAppM)

-- | Run an AppM action
runAppM :: AgentEnv -> AppM a -> IO a
runAppM env action = runReaderT (unAppM action) env

-- | Katip instance for AppM
instance Katip AppM where
  getLogEnv = asks aeLogEnv
  localLogEnv f = AppM . local (\env -> env { aeLogEnv = f (aeLogEnv env) }) . unAppM

-- | KatipContext instance for AppM
instance KatipContext AppM where
  getKatipContext = asks aeContext
  localKatipContext f = AppM . local (\env -> env { aeContext = f (aeContext env) }) . unAppM
  getKatipNamespace = asks aeNamespace
  localKatipNamespace f = AppM . local (\env -> env { aeNamespace = f (aeNamespace env) }) . unAppM

-- | Log a message from plain IO using a LogEnv
-- Use this for background threads that don't run in AppM
logIO :: LogEnv -> Namespace -> Severity -> LogStr -> IO ()
logIO logEnv ns severity msg =
  runKatipContextT logEnv () ns $ logFM severity msg

-- | Log debug message from IO
logDebugIO :: LogEnv -> Namespace -> LogStr -> IO ()
logDebugIO logEnv ns = logIO logEnv ns DebugS

-- | Log info message from IO
logInfoIO :: LogEnv -> Namespace -> LogStr -> IO ()
logInfoIO logEnv ns = logIO logEnv ns InfoS

-- | Log warning message from IO
logWarnIO :: LogEnv -> Namespace -> LogStr -> IO ()
logWarnIO logEnv ns = logIO logEnv ns WarningS

-- | Log error message from IO
logErrorIO :: LogEnv -> Namespace -> LogStr -> IO ()
logErrorIO logEnv ns = logIO logEnv ns ErrorS
