-- scape/src/Scape/Protocol/Types.hs
module Scape.Protocol.Types
  ( -- * Identifiers
    SessionId (..)
  , VMId (..)
  , CommandId (..)
  , TemplateId (..)

    -- * Common types
  , ExitCode (..)
  , Token (..)
  , IPv4 (..)

    -- * Resource limits
  , ResourceLimits (..)
  , defaultResourceLimits

    -- * Egress policy
  , EgressPolicy (..)
  , egressLLMProviders
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))

-- | Unique session identifier (user-facing)
newtype SessionId = SessionId { unSessionId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Internal VM identifier
newtype VMId = VMId { unVMId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Command execution identifier
newtype CommandId = CommandId { unCommandId :: UUID }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

instance FromHttpApiData CommandId where
  parseUrlPiece t = CommandId <$> maybe (Left "Invalid UUID") Right (UUID.fromText t)

instance ToHttpApiData CommandId where
  toUrlPiece (CommandId u) = UUID.toText u

-- | Template identifier (e.g., "python-3.12", "nodejs-22")
newtype TemplateId = TemplateId { unTemplateId :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Process exit code
newtype ExitCode = ExitCode { unExitCode :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Num)

-- | Authentication token for stream endpoints
newtype Token = Token { unToken :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

-- | IPv4 address as text (e.g., "10.99.0.42")
newtype IPv4 = IPv4 { unIPv4 :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Resource limits for a sandbox
data ResourceLimits = ResourceLimits
  { rlMemoryMB      :: !Int    -- ^ Memory limit in MB
  , rlCPUPercent    :: !Int    -- ^ CPU quota (100 = 1 core)
  , rlPidsMax       :: !Int    -- ^ Maximum number of processes
  , rlDiskMB        :: !Int    -- ^ Writable disk quota in MB
  , rlTimeoutSec    :: !Int    -- ^ Maximum session duration
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

defaultResourceLimits :: ResourceLimits
defaultResourceLimits = ResourceLimits
  { rlMemoryMB   = 512
  , rlCPUPercent = 100
  , rlPidsMax    = 256
  , rlDiskMB     = 100
  , rlTimeoutSec = 3600
  }

-- | Network egress policy
data EgressPolicy
  = EgressAllowAll                -- ^ Full internet access
  | EgressAllowDomains [Text]     -- ^ Allowlist specific domains (SNI)
  | EgressDenyAll                 -- ^ No network access
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Common preset: LLM API providers
egressLLMProviders :: EgressPolicy
egressLLMProviders = EgressAllowDomains
  [ "api.openai.com"
  , "api.anthropic.com"
  , "api.cohere.ai"
  , "generativelanguage.googleapis.com"
  , "api.mistral.ai"
  , "api.groq.com"
  ]
