-- scape/src/Scape/Protocol/Control.hs
module Scape.Protocol.Control
  ( -- * Commands (orchestrator -> agent)
    ControlCommand (..)
  , RunCommandArgs (..)
  , WriteFileArgs (..)
  , InjectSecretsArgs (..)
  , SecretScope (..)

    -- * Events (agent -> orchestrator)
  , ControlEvent (..)
  , OutputChannel (..)
  , AgentHealth (..)
  , AgentMetricsData (..)

    -- * Results
  , CommandResult (..)
  , FileResult (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Scape.Protocol.Types

-- | Commands sent from orchestrator to agent via vsock
data ControlCommand
  = CmdPing
  | CmdRunCommand !RunCommandArgs
  | CmdCancelCommand !CommandId
  | CmdWriteFile !WriteFileArgs
  | CmdReadFile !FilePath
  | CmdListDir !FilePath
  | CmdDeletePath !FilePath !Bool  -- ^ path, recursive
  | CmdInjectSecrets !InjectSecretsArgs
  | CmdPrepareStream !CommandId !Token  -- ^ Prepare for WS/SSE connection
  | CmdGetMetrics
  | CmdRefreshConfig  -- ^ Re-fetch MMDS and update orchestrator connection config
  | CmdShutdown
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data RunCommandArgs = RunCommandArgs
  { rcaCommandId  :: !CommandId
  , rcaCommand    :: !Text
  , rcaArgs       :: ![Text]
  , rcaEnv        :: !(Map Text Text)
  , rcaSecrets    :: !(Map Text Text)  -- ^ Never logged
  , rcaWorkdir    :: !FilePath
  , rcaStdin      :: !(Maybe Text)  -- ^ Base64 encoded if binary
  , rcaTimeout    :: !Int              -- ^ Seconds
  , rcaInteractive :: !Bool            -- ^ Allocate PTY
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data WriteFileArgs = WriteFileArgs
  { wfaPath    :: !FilePath
  , wfaContent :: !Text  -- ^ Base64 encoded
  , wfaMode    :: !(Maybe Int)  -- ^ Unix permissions (e.g., 0o644)
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data InjectSecretsArgs = InjectSecretsArgs
  { isaSecrets :: !(Map Text Text)
  , isaScope   :: !SecretScope
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data SecretScope
  = ScopeSession     -- ^ Available for all subsequent commands
  | ScopeNextCommand -- ^ Available only for next command, then cleared
  | ScopeFile !FilePath  -- ^ Write to file with restricted permissions
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Events sent from agent to orchestrator via vsock
data ControlEvent
  = EvPong
  | EvAgentReady !Text              -- ^ instanceId - sent on connection
  | EvStreamReady !CommandId !Int !Token  -- ^ commandId, port, token
  | EvCommandStarted !CommandId
  | EvCommandOutput !CommandId !OutputChannel !Text  -- ^ commandId, channel, base64-encoded data
  | EvCommandComplete !CommandId !ExitCode !Double  -- ^ duration in seconds
  | EvCommandError !CommandId !Text
  | EvHealthStatus !AgentHealth
  | EvMetrics !AgentMetricsData
  | EvConfigRefreshed !(Maybe Text)  -- ^ MMDS instance-id if successful, Nothing if failed
  | EvShuttingDown
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Output channel for command output
data OutputChannel = Stdout | Stderr
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data AgentHealth = AgentHealth
  { ahHealthy     :: !Bool
  , ahUptime      :: !Double    -- ^ Seconds
  , ahMemoryUsed  :: !Int       -- ^ KB
  , ahActiveCommands :: !Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data AgentMetricsData = AgentMetricsData
  { amdCommandsTotal   :: !Int
  , amdCommandsActive  :: !Int
  , amdBytesStreamed   :: !Int
  , amdErrorsTotal     :: !Int
  , amdWsConnections   :: !Int
  , amdSseConnections  :: !Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Result of a completed command
data CommandResult = CommandResult
  { crExitCode :: !ExitCode
  , crStdout   :: !Text  -- ^ Base64 encoded
  , crStderr   :: !Text  -- ^ Base64 encoded
  , crDuration :: !Double  -- ^ Seconds
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Result of a file operation
data FileResult
  = FileOk
  | FileContent !Text  -- ^ Base64 encoded
  | FileListing ![(Text, Bool, Int, UTCTime)]  -- ^ name, isDir, size, modTime
  | FileError !Text
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
