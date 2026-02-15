-- | Observation protocol types (Agent → Orchestrator)
--
-- Observations are published via NATS to observation subjects.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Scape.Protocol.Observation
  ( -- * Observation envelope
    Observation (..)

    -- * Event types
  , AgentInfo (..)
  , OutputEvent (..)
  , CompleteEvent (..)
  , ErrorEvent (..)
  , MetricsEvent (..)
  , FileContentEvent (..)
  , FileWrittenEvent (..)
  , SecretsInjectedEvent (..)

    -- * Channel type
  , Channel (..)

    -- * Re-exports
  , IPv4
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Net.IPv4 (IPv4)

import Scape.Protocol.Command (CommandId)

-- | Observations sent from agent to orchestrator
data Observation
  = ObsReady !AgentInfo
  | ObsOutput !OutputEvent
  | ObsComplete !CompleteEvent
  | ObsError !ErrorEvent
  | ObsMetrics !MetricsEvent
  | ObsFileContent !FileContentEvent
  | ObsFileWritten !FileWrittenEvent
  | ObsSecretsInjected !SecretsInjectedEvent
  | ObsShuttingDown
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Agent information sent on startup
data AgentInfo = AgentInfo
  { instanceId :: !Text
  , version    :: !Text
  , startedAt  :: !UTCTime
  , ipAddress  :: !IPv4    -- ^ Agent's IP address (discovered via network-info)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Output channel (stdout or stderr)
data Channel = Stdout | Stderr
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Command output chunk
data OutputEvent = OutputEvent
  { commandId :: !CommandId
  , channel   :: !Channel
  , content   :: !Text        -- ^ Base64 encoded output data
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Command completion event
data CompleteEvent = CompleteEvent
  { commandId :: !CommandId
  , exitCode  :: !Int
  , duration  :: !Double      -- ^ Duration in seconds
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Error event (command or agent-level)
data ErrorEvent = ErrorEvent
  { commandId :: !(Maybe CommandId)  -- ^ Nothing for agent-level errors
  , message   :: !Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Periodic metrics from agent
data MetricsEvent = MetricsEvent
  { commandsTotal  :: !Int
  , commandsActive :: !Int
  , memoryUsedKb   :: !Int
  , cpuPercent     :: !Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | File content read from agent filesystem
data FileContentEvent = FileContentEvent
  { path    :: !FilePath
  , content :: !Text        -- ^ Base64 encoded content
  , size    :: !Int         -- ^ Actual size in bytes
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Confirmation that file was written
data FileWrittenEvent = FileWrittenEvent
  { path :: !FilePath
  , size :: !Int            -- ^ Bytes written
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Confirmation that secrets were injected
data SecretsInjectedEvent = SecretsInjectedEvent
  { count     :: !Int       -- ^ Number of secrets written
  , targetDir :: !FilePath
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
