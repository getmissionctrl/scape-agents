-- | Command protocol types (Orchestrator → Agent)
--
-- Commands are sent via NATS to agent-specific subjects.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Scape.Protocol.Command
  ( -- * Command envelope
    Command (..)

    -- * Command types
  , ExecRequest (..)
  , WriteFileRequest (..)
  , ReadFileRequest (..)
  , ListDirRequest (..)
  , SecretsRequest (..)

    -- * Identifiers (re-exported from Types)
  , CommandId (..)
  , newCommandId
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

import Scape.Protocol.Types (CommandId(..))

-- | Generate a new random command ID
newCommandId :: IO CommandId
newCommandId = CommandId <$> UUID.nextRandom

-- | Commands sent from orchestrator to agent
data Command
  = CmdExec !ExecRequest
  | CmdCancel !CommandId
  | CmdWriteFile !WriteFileRequest
  | CmdReadFile !ReadFileRequest
  | CmdListDir !ListDirRequest
  | CmdInjectSecrets !SecretsRequest
  | CmdPing
  | CmdShutdown
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Execute a command in the sandbox
data ExecRequest = ExecRequest
  { id      :: !CommandId
  , command :: !Text           -- ^ Command to execute
  , args    :: ![Text]         -- ^ Arguments
  , env     :: !(Map Text Text) -- ^ Environment variables
  , workdir :: !FilePath       -- ^ Working directory
  , stdin   :: !(Maybe Text)   -- ^ Base64 encoded stdin data
  , timeout :: !Int            -- ^ Timeout in seconds
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Write a file to the sandbox
data WriteFileRequest = WriteFileRequest
  { path    :: !FilePath
  , content :: !Text           -- ^ Base64 encoded content
  , mode    :: !(Maybe Int)    -- ^ Unix permissions (e.g., 0o644)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Read a file from the sandbox
data ReadFileRequest = ReadFileRequest
  { path     :: !FilePath
  , maxBytes :: !(Maybe Int)   -- ^ Limit bytes read
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | List directory contents
data ListDirRequest = ListDirRequest
  { path :: !FilePath
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Inject secrets into the sandbox
data SecretsRequest = SecretsRequest
  { secrets   :: !(Map Text Text)  -- ^ Secret name → value pairs
  , targetDir :: !FilePath         -- ^ Target directory (e.g., "/run/scape/secrets")
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
