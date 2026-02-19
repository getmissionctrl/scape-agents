-- scape/src/Scape/Agent/Executor.hs
--
-- Command execution with streaming output support.
-- Supports both callback-based (NATS) and TQueue-based (legacy) output.
module Scape.Agent.Executor
  ( -- * Synchronous execution
    executeCommandSync
  , CommandResult (..)
    -- * Asynchronous execution
  , executeCommandAsync
  , cancelCommand
    -- * Callback-based execution (for NATS)
  , executeCommandWithCallback
  , OutputCallback
    -- * PTY/Interactive execution
  , executeCommandPty
    -- * User isolation
  , wrapAsOperator
  , wrapForOperator
    -- * Utilities
  , decodeBase64
  , encodeBase64
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (async, wait, race)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, try)
import Control.Monad (forM_, forever, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Machine (ProcessT, MachineT(..), Step(..), runT_, repeatedly, await, (~>))
import Control.Monad.Trans (lift)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Optics.Core ((%~), (&))
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose, hSetBinaryMode)
import System.Posix.User (getEffectiveUserID)
import System.Process
import qualified System.Timeout as Timeout

import Scape.Agent.State
import Scape.Protocol.Types (CommandId, Token(..))
import qualified Scape.Protocol.Types as P
import Scape.Protocol.Control (OutputChannel(..))
import Scape.Protocol.AISDK (ChatEvent(..))

-- | Result of command execution
data CommandResult = CommandResult
  { exitCode :: !P.ExitCode
  , stdout   :: !ByteString
  , stderr   :: !ByteString
  , duration :: !Double
  }
  deriving stock (Show, Generic)

-- | Pure logic: wrap a command for operator execution.
-- Given a UID, determines whether to wrap with runuser.
wrapForOperator :: Word32 -> Text -> [Text] -> (Text, [Text])
wrapForOperator uid cmd args
  | uid == 0  = ("runuser", ["-u", "operator", "--", cmd] ++ args)
  | otherwise = (cmd, args)

-- | Wrap a command to run as the operator user via runuser.
-- Only wraps when running as root (uid 0). In non-root contexts
-- (e.g., tests), runs the command directly.
wrapAsOperator :: Text -> [Text] -> IO (Text, [Text])
wrapAsOperator cmd args = do
  uid <- getEffectiveUserID
  pure $ wrapForOperator (fromIntegral uid) cmd args

-- | Execute a command synchronously (blocks until completion)
executeCommandSync
  :: AgentState
  -> CommandId
  -> Text           -- ^ Command
  -> [Text]         -- ^ Arguments
  -> [(Text, Text)] -- ^ Environment
  -> FilePath       -- ^ Working directory
  -> Maybe ByteString -- ^ Stdin
  -> Int            -- ^ Timeout in seconds
  -> IO CommandResult
executeCommandSync _state _cmdId cmd args env workdir mstdin timeoutSec = do
  startTime <- getCurrentTime

  (wrappedCmd, wrappedArgs) <- wrapAsOperator cmd args
  let procSpec = (proc (T.unpack wrappedCmd) (map T.unpack wrappedArgs))
        { cwd = Just workdir
        , env = Just $ map (\(k, v) -> (T.unpack k, T.unpack v)) env
        , std_in = maybe NoStream (const CreatePipe) mstdin
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

  result <- Timeout.timeout (timeoutSec * 1000000) $ do
    (mhin, Just hout, Just herr, ph) <- createProcess procSpec

    -- Send stdin if provided
    case (mhin, mstdin) of
      (Just hin, Just stdin) -> do
        BS.hPut hin stdin
        hClose hin
      _ -> pure ()

    -- Read outputs
    hSetBinaryMode hout True
    hSetBinaryMode herr True

    stdoutAsync <- async $ BS.hGetContents hout
    stderrAsync <- async $ BS.hGetContents herr

    procExitCode <- waitForProcess ph
    stdoutData <- wait stdoutAsync
    stderrData <- wait stderrAsync

    endTime <- getCurrentTime
    let dur = realToFrac $ diffUTCTime endTime startTime

    pure CommandResult
      { exitCode = case procExitCode of
          ExitSuccess -> P.ExitCode 0
          ExitFailure n -> P.ExitCode n
      , stdout = stdoutData
      , stderr = stderrData
      , duration = dur
      }

  case result of
    Just r -> pure r
    Nothing -> do
      -- Timeout - return error
      endTime <- getCurrentTime
      let dur = realToFrac $ diffUTCTime endTime startTime
      pure CommandResult
        { exitCode = P.ExitCode (-1)
        , stdout = ""
        , stderr = "Command timed out after " <> TE.encodeUtf8 (T.pack $ show timeoutSec) <> " seconds"
        , duration = dur
        }

-- | Execute a command asynchronously with streaming
executeCommandAsync
  :: AgentState
  -> CommandId
  -> Text           -- ^ Command
  -> [Text]         -- ^ Arguments
  -> [(Text, Text)] -- ^ Environment
  -> FilePath       -- ^ Working directory
  -> Int            -- ^ Timeout in seconds
  -> Token          -- ^ Token for stream authentication
  -> IO ()
executeCommandAsync state cmdId cmd args env workdir timeoutSec tkn = void $ forkIO $ do
  startTime <- getCurrentTime

  (wrappedCmd, wrappedArgs) <- wrapAsOperator cmd args
  -- Use Nothing for env when empty to inherit parent environment (includes PATH)
  -- Otherwise set explicit environment variables
  let envSetting = if null env
                   then Nothing
                   else Just $ map (\(k, v) -> (T.unpack k, T.unpack v)) env
      procSpec = (proc (T.unpack wrappedCmd) (map T.unpack wrappedArgs))
        { cwd = Just workdir
        , env = envSetting
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

  -- Try to create process - may fail if command not found
  procResult <- try $ createProcess procSpec
  case procResult of
    Left (e :: SomeException) -> do
      -- Process creation failed (e.g., command not found)
      endTime <- getCurrentTime
      let dur = realToFrac $ diffUTCTime endTime startTime
      atomically $ do
        writeTQueue (eventQueue state) $
          CECommandError cmdId ("Failed to start process: " <> T.pack (show e))
        writeTQueue (eventQueue state) $
          CECommandComplete cmdId (-1) dur
        modifyTVar' (metrics state) $ \m -> m & #errorsTotal %~ (+ 1)
      return ()

    Right (Just hin, Just hout, Just herr, ph) -> do
      -- Create stdin channel for interactive input
      stdinCh <- newTChanIO

      -- Register command
      let cmdState = CommandState
            { processHandle = ph
            , startedAt = startTime
            , timeout = timeoutSec
            , stdinChan = stdinCh
            , token = tkn
            }
      registerCommand state cmdId cmdState

      -- Stdin forwarder (from WebSocket clients)
      stdinThread <- forkIO $ stdinForwarder stdinCh hin

      -- Stdout reader (fans out to subscribers) - use async so we can wait for completion
      hSetBinaryMode hout True
      stdoutAsync <- async $ streamOutput state cmdId Stdout hout

      -- Stderr reader (also fans out to subscribers)
      hSetBinaryMode herr True
      stderrAsync <- async $ streamOutput state cmdId Stderr herr

      -- Wait for completion or timeout
      result <- race
        (threadDelay (timeoutSec * 1000000))
        (waitForProcess ph)

      -- Kill stdin forwarder (no longer needed)
      killThread stdinThread

      -- Close stdin handle
      catch (hClose hin) (\(_ :: SomeException) -> pure ())

      -- Wait for output readers to drain the pipes (they'll exit when handles are closed by process)
      -- Use a short timeout to avoid hanging if something goes wrong
      _ <- race (threadDelay 1000000) (wait stdoutAsync)  -- 1 second timeout
      _ <- race (threadDelay 1000000) (wait stderrAsync)

      -- Close output handles (in case they weren't already closed)
      catch (hClose hout) (\(_ :: SomeException) -> pure ())
      catch (hClose herr) (\(_ :: SomeException) -> pure ())

      endTime <- getCurrentTime
      let dur = realToFrac $ diffUTCTime endTime startTime

      let procExitCode = case result of
            Left () -> P.ExitCode (-1)  -- Timeout
            Right ExitSuccess -> P.ExitCode 0
            Right (ExitFailure n) -> P.ExitCode n

      -- Notify completion
      atomically $ writeTQueue (eventQueue state) $
        CECommandComplete cmdId (P.unExitCode procExitCode) dur

      -- Unregister
      unregisterCommand state cmdId

    Right _ -> do
      -- Unexpected pipe configuration
      endTime <- getCurrentTime
      let dur = realToFrac $ diffUTCTime endTime startTime
      atomically $ do
        writeTQueue (eventQueue state) $
          CECommandError cmdId "Unexpected process pipe configuration"
        writeTQueue (eventQueue state) $
          CECommandComplete cmdId (-1) dur
        modifyTVar' (metrics state) $ \m -> m & #errorsTotal %~ (+ 1)

-- | Forward stdin from channel to handle
stdinForwarder :: TChan ByteString -> Handle -> IO ()
stdinForwarder chan handle = forever $ do
  chunk <- atomically $ readTChan chan
  catch (BS.hPut handle chunk) (\(_ :: SomeException) -> pure ())

-- | Stream output to all subscribers and the event queue
streamOutput :: AgentState -> CommandId -> OutputChannel -> Handle -> IO ()
streamOutput state cmdId channel handle = loop
  where
    loop = do
      chunk <- BS.hGetSome handle 4096
      if BS.null chunk
        then pure ()
        else do
          let textChunk = decodeUtf8Lenient chunk

          -- Send to WS/SSE subscribers
          subs <- getSubscribers state cmdId
          forM_ subs $ \sub -> case sub of
            WSSubscriber chan -> atomically $ writeTChan chan chunk
            SSESubscriber chan -> atomically $
              writeTChan chan (ChatTextDelta textChunk)

          -- Send to control event queue (for scape-ctl)
          atomically $ writeTQueue (eventQueue state) $
            CECommandOutput cmdId channel textChunk

          -- Update metrics
          atomically $ modifyTVar' (metrics state) $ \m ->
            m & #bytesStreamed %~ (+ BS.length chunk)
          loop

    decodeUtf8Lenient :: ByteString -> Text
    decodeUtf8Lenient = TE.decodeUtf8With (\_ _ -> Just '?')

-- | Callback type for output streaming
--
-- Called with (channel, raw bytes) for each output chunk.
-- The callback is responsible for encoding/publishing (e.g., to NATS).
type OutputCallback = OutputChannel -> ByteString -> IO ()

-- | Completion callback type
--
-- Called with (exit code, duration in seconds).
type CompletionCallback = Int -> Double -> IO ()

-- | Error callback type
--
-- Called with error message.
type ErrorCallback = Text -> IO ()

-- | Execute a command with callback-based output streaming
--
-- This is the NATS-friendly version that uses callbacks instead of
-- TQueues for output delivery. Suitable for use with Agent.Nats module.
executeCommandWithCallback
  :: CommandId
  -> Text           -- ^ Command
  -> [Text]         -- ^ Arguments
  -> [(Text, Text)] -- ^ Environment
  -> FilePath       -- ^ Working directory
  -> Maybe ByteString -- ^ Stdin data
  -> Int            -- ^ Timeout in seconds
  -> OutputCallback     -- ^ Called for each stdout/stderr chunk
  -> CompletionCallback -- ^ Called on successful completion
  -> ErrorCallback      -- ^ Called on error (process creation failure, timeout)
  -> IO ()
executeCommandWithCallback _cmdId cmd args env workdir mStdin timeoutSec onOutput onComplete onError = void $ forkIO $ do
  startTime <- getCurrentTime

  (wrappedCmd, wrappedArgs) <- wrapAsOperator cmd args
  -- Use Nothing for env when empty to inherit parent environment (includes PATH)
  let envSetting = if null env
                   then Nothing
                   else Just $ map (\(k, v) -> (T.unpack k, T.unpack v)) env
      procSpec = (proc (T.unpack wrappedCmd) (map T.unpack wrappedArgs))
        { cwd = Just workdir
        , env = envSetting
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

  -- Try to create process
  procResult <- try $ createProcess procSpec
  case procResult of
    Left (e :: SomeException) -> do
      -- Process creation failed (e.g., command not found)
      endTime <- getCurrentTime
      let dur = realToFrac $ diffUTCTime endTime startTime
      onError ("Failed to start process: " <> T.pack (show e))
      onComplete (-1) dur

    Right (Just hin, Just hout, Just herr, ph) -> do
      -- Write stdin if provided
      case mStdin of
        Just stdin -> do
          catch (BS.hPut hin stdin >> hClose hin) $ \(_ :: SomeException) -> pure ()
        Nothing ->
          catch (hClose hin) $ \(_ :: SomeException) -> pure ()

      -- Stream stdout with callback
      hSetBinaryMode hout True
      stdoutAsync <- async $ streamWithCallback Stdout hout onOutput

      -- Stream stderr with callback
      hSetBinaryMode herr True
      stderrAsync <- async $ streamWithCallback Stderr herr onOutput

      -- Wait for completion or timeout
      result <- race
        (threadDelay (timeoutSec * 1000000))
        (waitForProcess ph)

      -- Wait for output readers to drain (with timeout)
      _ <- race (threadDelay 1000000) (wait stdoutAsync)
      _ <- race (threadDelay 1000000) (wait stderrAsync)

      -- Close output handles
      catch (hClose hout) $ \(_ :: SomeException) -> pure ()
      catch (hClose herr) $ \(_ :: SomeException) -> pure ()

      endTime <- getCurrentTime
      let dur = realToFrac $ diffUTCTime endTime startTime

      case result of
        Left () -> do
          -- Timeout
          catch (terminateProcess ph) $ \(_ :: SomeException) -> pure ()
          onError "Command timed out"
          onComplete (-1) dur
        Right ExitSuccess ->
          onComplete 0 dur
        Right (ExitFailure n) ->
          onComplete n dur

    Right _ -> do
      -- Unexpected pipe configuration
      endTime <- getCurrentTime
      let dur = realToFrac $ diffUTCTime endTime startTime
      onError "Unexpected process pipe configuration"
      onComplete (-1) dur

-- | Stream output from a handle using machines, calling callback for each chunk
streamWithCallback :: OutputChannel -> Handle -> OutputCallback -> IO ()
streamWithCallback channel handle callback =
  runT_ $ handleSource handle ~> sinkIO (callback channel)

-- | A machine source that reads chunks from a handle until EOF
--
-- Yields ByteString chunks of up to 4096 bytes.
-- Terminates when EOF is reached (empty read).
handleSource :: Handle -> MachineT IO k ByteString
handleSource handle = MachineT $ do
  chunk <- BS.hGetSome handle 4096
  if BS.null chunk
    then pure Stop
    else pure $ Yield chunk (handleSource handle)

-- | A machine sink that runs an IO action for each input
--
-- Terminates when upstream is exhausted.
sinkIO :: (a -> IO ()) -> ProcessT IO a b
sinkIO f = repeatedly $ await >>= lift . f

-- | Execute a command with PTY allocation (interactive)
-- Note: Full PTY implementation requires the posix-pty package.
-- This placeholder uses the existing async execution which supports
-- interactive stdin via the stdin channel, just without proper terminal emulation.
-- For true PTY support (terminal size, job control, etc.), add posix-pty dependency.
executeCommandPty
  :: AgentState
  -> CommandId
  -> Text           -- ^ Command
  -> [Text]         -- ^ Arguments
  -> [(Text, Text)] -- ^ Environment
  -> FilePath       -- ^ Working directory
  -> Int            -- ^ Timeout in seconds
  -> Token          -- ^ Token for stream authentication
  -> IO ()
executeCommandPty state cmdId cmd args env workdir timeoutSec tkn = do
  -- For now, fall back to async execution which handles stdin
  -- TODO: Add posix-pty dependency and implement proper PTY:
  --   1. Use System.Posix.Pty.spawnWithPty to get master/slave handles
  --   2. Forward master FD output to subscribers
  --   3. Forward stdin channel input to master FD
  --   4. Handle window resize events
  executeCommandAsync state cmdId cmd args env workdir timeoutSec tkn

-- | Cancel a running command
cancelCommand :: AgentState -> CommandId -> IO ()
cancelCommand state cmdId = do
  mCmd <- getCommand state cmdId
  case mCmd of
    Nothing -> pure ()
    Just cmdState -> do
      terminateProcess (processHandle cmdState)
      -- Send error event
      atomically $ writeTQueue (eventQueue state) $
        CECommandError cmdId "Command cancelled"
      unregisterCommand state cmdId

-- | Decode base64 text to bytes
decodeBase64 :: Text -> Either String ByteString
decodeBase64 = B64.decode . TE.encodeUtf8

-- | Encode bytes to base64 text
encodeBase64 :: ByteString -> Text
encodeBase64 = TE.decodeUtf8 . B64.encode
