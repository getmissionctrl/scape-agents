-- | Background metrics collection and publishing loop
module Scape.Agent.Metrics
  ( metricsLoop
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (readTVarIO, atomically, modifyTVar')
import Control.Exception (SomeException, catch)
import Control.Monad (forever)

import Optics.Core ((.~), (&))

import Scape.Agent.State (AgentState (..), AgentMetrics (..))
import Scape.Agent.System (SystemMetrics (..), collectSystemMetrics)
import qualified Scape.Protocol.Observation as Obs

-- | Periodically collect system metrics, update agent state, and publish.
--
-- Runs every 10 seconds. Catches and logs any exceptions so the loop never dies.
-- The publish callback is typically @publishMetrics publisher@ from Scape.Agent.Nats.
metricsLoop :: AgentState -> (Obs.MetricsEvent -> IO ()) -> IO ()
metricsLoop state publishEvent = forever $ do
  threadDelay 10_000_000  -- 10 seconds
  collectAndPublish state publishEvent
    `catch` \(e :: SomeException) ->
      putStrLn $ "[Metrics] Error: " ++ show e

collectAndPublish :: AgentState -> (Obs.MetricsEvent -> IO ()) -> IO ()
collectAndPublish state publishEvent = do
  -- Collect system metrics (destructure to avoid DuplicateRecordFields)
  SystemMetrics
    { cpuPercent  = sysCpu
    , memUsedKb   = sysMemUsed
    , memTotalKb  = sysMemTotal
    , netRxBytes  = sysNetRx
    , netTxBytes  = sysNetTx
    , diskUsedKb  = sysDiskUsed
    , diskTotalKb = sysDiskTotal
    } <- collectSystemMetrics

  -- Update AgentState with fresh system metrics
  atomically $ modifyTVar' (metrics state) $ \m ->
    m & #cpuPercent  .~ sysCpu
      & #memUsedKb   .~ sysMemUsed
      & #memTotalKb  .~ sysMemTotal
      & #netRxBytes  .~ sysNetRx
      & #netTxBytes  .~ sysNetTx
      & #diskUsedKb  .~ sysDiskUsed
      & #diskTotalKb .~ sysDiskTotal

  -- Read the full snapshot and destructure to avoid DuplicateRecordFields issues
  AgentMetrics
    { commandsTotal  = cmdsTotal
    , commandsActive = cmdsActive
    , errorsTotal    = errsTotal
    , cpuPercent     = cpu
    , memUsedKb      = memUsed
    , memTotalKb     = memTotal
    , netRxBytes     = netRx
    , netTxBytes     = netTx
    , diskUsedKb     = diskUsed
    , diskTotalKb    = diskTotal
    } <- readTVarIO (metrics state)

  -- Publish via NATS
  publishEvent Obs.MetricsEvent
    { Obs.cpuPercent     = cpu
    , Obs.memUsedKb      = memUsed
    , Obs.memTotalKb     = memTotal
    , Obs.netRxBytes     = netRx
    , Obs.netTxBytes     = netTx
    , Obs.diskUsedKb     = diskUsed
    , Obs.diskTotalKb    = diskTotal
    , Obs.commandsTotal  = cmdsTotal
    , Obs.commandsActive = cmdsActive
    , Obs.errorsTotal    = errsTotal
    }
