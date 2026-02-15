-- | NATS test harness
--
-- Provides a bracket-based setup that starts a local NATS server
-- for integration testing without requiring the dev environment.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Nats
  ( -- * Test harness
    NatsTestEnv(..)
  , withNatsServer
  , withNatsServerSpec

    -- * Helpers
  , findFreePort
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket
import System.IO (Handle, hClose)
import System.Process
  ( CreateProcess(..)
  , StdStream(..)
  , ProcessHandle
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )
import Test.Hspec (Spec, SpecWith, aroundAll)

-- | Test environment with a running NATS server
data NatsTestEnv = NatsTestEnv
  { natsPort    :: !Int           -- ^ Port the server is listening on
  , natsUrl     :: !Text          -- ^ Full URL like "nats://localhost:4222"
  , natsProcess :: !ProcessHandle -- ^ Process handle for cleanup
  , natsStderr  :: !Handle        -- ^ Stderr handle for debugging
  }

-- | Start a NATS server on a free port with no auth
--
-- Uses bracket pattern for safe cleanup.
withNatsServer :: (NatsTestEnv -> IO a) -> IO a
withNatsServer = bracket startNatsServer stopNatsServer

-- | Hspec wrapper for aroundAll
--
-- Usage:
--   spec = withNatsServerSpec $ \env -> do
--     it "test" $ do
--       -- use env.natsUrl etc
withNatsServerSpec :: SpecWith NatsTestEnv -> Spec
withNatsServerSpec = aroundAll withNatsServer

-- | Find a free port by binding to port 0
findFreePort :: IO Int
findFreePort = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  port <- socketPort sock
  close sock
  pure $ fromIntegral port

-- | Start NATS server on a free port
startNatsServer :: IO NatsTestEnv
startNatsServer = do
  port <- findFreePort
  let natsUrl = "nats://localhost:" <> T.pack (show port)

  -- Start nats-server with minimal config (no auth, no jetstream for speed)
  let cp = (proc "nats-server" ["-p", show port, "-D"])
        { std_out = CreatePipe
        , std_err = CreatePipe
        }

  (_, _, Just hErr, ph) <- createProcess cp

  -- Wait for server to be ready (check by connecting)
  waitForReady port 50  -- 50 * 100ms = 5 seconds max

  pure NatsTestEnv
    { natsPort = port
    , natsUrl = natsUrl
    , natsProcess = ph
    , natsStderr = hErr
    }

-- | Wait for NATS server to accept connections
waitForReady :: Int -> Int -> IO ()
waitForReady port retries = do
  result <- try $ do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
    close sock
  case result of
    Right () -> pure ()
    Left (_ :: SomeException)
      | retries > 0 -> do
          threadDelay 100000  -- 100ms
          waitForReady port (retries - 1)
      | otherwise -> error $ "NATS server failed to start on port " <> show port

-- | Stop NATS server and cleanup
stopNatsServer :: NatsTestEnv -> IO ()
stopNatsServer NatsTestEnv{..} = do
  terminateProcess natsProcess
  _ <- waitForProcess natsProcess
  hClose natsStderr
