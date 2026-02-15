-- scape/src/Scape/Agent/MMDS.hs
--
-- Firecracker MMDS (Microvm Metadata Service) client for agent config.
-- Fetches instance config from http://169.254.169.254/scape on boot.
-- Uses MMDS V1 (no token authentication needed).
--
-- Types are defined in Scape.Protocol.MMDS and re-exported here.
module Scape.Agent.MMDS
  ( -- * MMDS Client
    fetchMMDSConfig
  , mmdsUrl

    -- * Re-exports from Protocol.MMDS
  , MMDSConfig (..)
  , MMDSData (..)
  , NatsConfig (..)
  , mkMMDSData
  , mkMMDSDataFromConfig
  , encodeMMDSData
  ) where

import Control.Exception (catch, SomeException)
import Data.Aeson (eitherDecode)
import Data.Text (Text)
import qualified Data.Text as T
import Katip (LogEnv, Namespace(..), Severity(..), logStr, runKatipT, logMsg)
import Network.HTTP.Client
  ( httpLbs
  , newManager
  , parseRequest
  , responseBody
  , responseStatus
  , defaultManagerSettings
  , managerResponseTimeout
  , responseTimeoutMicro
  , requestHeaders
  )
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header (hAccept)

-- Re-export protocol types
import Scape.Protocol.MMDS (MMDSConfig(..), MMDSData(..), NatsConfig(..), mkMMDSData, mkMMDSDataFromConfig, encodeMMDSData)

-- | MMDS URL for scape metadata
mmdsUrl :: String
mmdsUrl = "http://169.254.169.254/scape"

-- | Fetch configuration from MMDS with timeout
-- Returns Nothing if MMDS is unavailable (e.g., running on host without Firecracker)
fetchMMDSConfig :: LogEnv -> Int -> IO (Maybe MMDSConfig)
fetchMMDSConfig logEnv timeoutMs = catch doFetch handleError
  where
    ns = Namespace ["mmds"]

    logInfo msg = runKatipT logEnv $ logMsg ns InfoS (logStr msg)
    logWarn msg = runKatipT logEnv $ logMsg ns WarningS (logStr msg)
    logDebug msg = runKatipT logEnv $ logMsg ns DebugS (logStr msg)

    doFetch = do
      logDebug ("Fetching MMDS from " <> T.pack mmdsUrl :: Text)
      manager <- newManager defaultManagerSettings
        { managerResponseTimeout = responseTimeoutMicro (timeoutMs * 1000)
        }
      baseRequest <- parseRequest mmdsUrl
      -- MMDS V1 requires Accept: application/json to return JSON instead of directory listing
      let request = baseRequest { requestHeaders = [(hAccept, "application/json")] }
      response <- httpLbs request manager

      let status = statusCode (responseStatus response)
      if status == 200
        then case eitherDecode (responseBody response) of
          Right config -> do
            logInfo ("MMDS config loaded: instance-id=" <> config.instanceId :: Text)
            pure (Just config)
          Left err -> do
            logWarn ("Failed to parse MMDS response: " <> T.pack err :: Text)
            pure Nothing
        else do
          logWarn ("MMDS returned non-200 status: " <> T.pack (show status) :: Text)
          pure Nothing

    handleError :: SomeException -> IO (Maybe MMDSConfig)
    handleError e = do
      -- MMDS not available - this is expected when running on host without Firecracker
      logDebug ("MMDS not available (expected on host): " <> T.pack (take 100 $ show e) :: Text)
      pure Nothing
