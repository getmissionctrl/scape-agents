-- | System metrics collection from /proc and df
module Scape.Agent.System
  ( SystemMetrics (..)
  , collectSystemMetrics
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)
import System.Process (readProcess)

-- | System metrics snapshot
data SystemMetrics = SystemMetrics
  { cpuPercent  :: !Double
  , memUsedKb   :: !Int
  , memTotalKb  :: !Int
  , netRxBytes  :: !Int64
  , netTxBytes  :: !Int64
  , diskUsedKb  :: !Int
  , diskTotalKb :: !Int
  }
  deriving stock (Generic, Show)

-- | Collect all system metrics.
--
-- Reads CPU from /proc/stat (with a 100ms sample window),
-- memory from /proc/meminfo, network from /proc/net/dev,
-- and disk from @df -k /@.
collectSystemMetrics :: IO SystemMetrics
collectSystemMetrics = do
  cpu <- readCpuPercent
  (memUsed, memTotal) <- readMemInfo
  (rx, tx) <- readNetDev
  (diskUsed, diskTotal) <- readDiskUsage
  pure SystemMetrics
    { cpuPercent  = cpu
    , memUsedKb   = memUsed
    , memTotalKb  = memTotal
    , netRxBytes  = rx
    , netTxBytes  = tx
    , diskUsedKb  = diskUsed
    , diskTotalKb = diskTotal
    }

--------------------------------------------------------------------------------
-- CPU
--------------------------------------------------------------------------------

readCpuPercent :: IO Double
readCpuPercent = do
  s1 <- readCpuTimes
  threadDelay 100_000  -- 100ms
  s2 <- readCpuTimes
  let totalDelta = sum (zipWith (-) s2 s1)
      idleDelta  = (s2 !! 3) - (s1 !! 3)
  pure $ if totalDelta == 0
    then 0.0
    else 100.0 * (1.0 - fromIntegral idleDelta / fromIntegral totalDelta)
 where
  readCpuTimes :: IO [Int]
  readCpuTimes = do
    content <- readFile "/proc/stat"
    case lines content of
      (cpuLine : _) -> pure $ map read $ take 8 $ drop 1 $ words cpuLine
      []            -> pure $ replicate 8 0

--------------------------------------------------------------------------------
-- Memory
--------------------------------------------------------------------------------

readMemInfo :: IO (Int, Int)
readMemInfo = do
  content <- readFile "/proc/meminfo"
  let ls    = lines content
      total = parseMemLine "MemTotal:" ls
      avail = parseMemLine "MemAvailable:" ls
  pure (total - avail, total)
 where
  parseMemLine :: String -> [String] -> Int
  parseMemLine prefix ls =
    case filter (isPrefixOf prefix) ls of
      (line : _) -> case drop 1 (words line) of
        (val : _) -> read val
        []        -> 0
      []         -> 0

--------------------------------------------------------------------------------
-- Network
--------------------------------------------------------------------------------

readNetDev :: IO (Int64, Int64)
readNetDev = do
  content <- readFile "/proc/net/dev"
  let ls = lines content
      ethLines = filter (\l -> "eth0:" `isPrefixOf` dropWhile isSpace l) ls
  case ethLines of
    (line : _) -> do
      let parts = words $ drop 1 $ dropWhile (/= ':') line
          rx    = read (parts !! 0) :: Int64
          tx    = read (parts !! 8) :: Int64
      pure (rx, tx)
    [] -> pure (0, 0)

--------------------------------------------------------------------------------
-- Disk
--------------------------------------------------------------------------------

-- | Read disk usage via @df -k /@.
readDiskUsage :: IO (Int, Int)
readDiskUsage =
  (do
    output <- readProcess "df" ["-k", "/"] ""
    let ls = lines output
    case drop 1 ls of
      (line : _) -> do
        let fields  = words line
            totalKb = read (fields !! 1) :: Int
            usedKb  = read (fields !! 2) :: Int
        pure (usedKb, totalKb)
      [] -> pure (0, 0)
  ) `catch` \(_ :: SomeException) -> pure (0, 0)
