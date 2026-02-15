-- | MMDS (MicroVM Metadata Service) protocol types
--
-- Defines the data structure injected via Firecracker MMDS
-- and fetched by agents on boot.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Scape.Protocol.MMDS
  ( -- * Top-level MMDS data
    MMDSData (..)
  , MMDSConfig (..)

    -- * NATS configuration
  , NatsConfig (..)

    -- * Helper functions
  , mkMMDSData
  , mkMMDSDataFromConfig
  , encodeMMDSData
  ) where

import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Top-level MMDS data structure (what gets PUT to /mmds)
data MMDSData = MMDSData
  { scape :: !MMDSConfig
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Configuration fetched from MMDS /scape path
data MMDSConfig = MMDSConfig
  { instanceId :: !Text
  , nats       :: !NatsConfig
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | NATS connection configuration for agents
--
-- Credentials and subjects are scoped per-instance for isolation.
-- For JWT authentication, the orchestrator reads the .creds file and passes
-- the raw content via `credsFileContent`. The agent writes this directly to
-- a temp file, avoiding parsing/reconstruction bugs.
data NatsConfig = NatsConfig
  { url              :: !Text          -- ^ NATS server URL (e.g., "nats://10.99.0.1:4222")
  , user             :: !Text          -- ^ Username (for logging only, not auth)
  , cmdSubject       :: !Text          -- ^ Subject to subscribe for commands (e.g., "scape.cmd.e9066c")
  , obsPrefix        :: !Text          -- ^ Prefix for publishing observations (e.g., "scape.obs.e9066c")
  , announceSubject  :: !(Maybe Text)  -- ^ Subject to announce readiness with IP (for template builds)
  , credsFileContent :: !(Maybe Text)  -- ^ Raw .creds file content for JWT+NKey auth
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Construct MMDS data for injection via Firecracker API
--
-- This is a basic constructor that doesn't include credentials.
-- For production use with JWT auth, use `mkMMDSDataFromConfig` with
-- a NatsConfig that has `credsFileContent` set.
mkMMDSData
  :: Text       -- ^ Instance ID
  -> Text       -- ^ NATS URL
  -> Text       -- ^ NATS username (for logging only)
  -> MMDSData
mkMMDSData instId natsUrl natsUser = MMDSData
  { scape = MMDSConfig
      { instanceId = instId
      , nats = NatsConfig
          { url = natsUrl
          , user = natsUser
          , cmdSubject = "scape.cmd." <> instId
          , obsPrefix = "scape.obs." <> instId
          , announceSubject = Nothing  -- Normal instances don't announce
          , credsFileContent = Nothing -- No credentials in basic constructor
          }
      }
  }

-- | Construct MMDS data from an existing NatsConfig
mkMMDSDataFromConfig
  :: Text        -- ^ Instance ID
  -> NatsConfig  -- ^ Pre-configured NATS config
  -> MMDSData
mkMMDSDataFromConfig instId natsCfg = MMDSData
  { scape = MMDSConfig
      { instanceId = instId
      , nats = natsCfg
      }
  }

-- | Encode MMDS data for injection via Firecracker API
-- Use this to create the JSON payload for PUT /mmds
encodeMMDSData :: MMDSData -> LBS.ByteString
encodeMMDSData = encode
