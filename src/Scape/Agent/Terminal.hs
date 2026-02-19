-- | Terminal WebSocket endpoint
--
-- Spawns a PTY running a login shell as the operator user,
-- bridging it bidirectionally to a WebSocket connection.
--
-- Protocol:
--   Client -> Agent: raw text (terminal input), or JSON {"type":"resize","cols":N,"rows":M}
--   Agent -> Client: raw PTY output bytes
module Scape.Agent.Terminal
  ( terminalHandler
  , terminalShellCmd
  , ResizeMessage (..)
  ) where

import Control.Concurrent.Async (race_)
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forever)
import Data.Aeson (FromJSON(..), withObject, (.:))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Word (Word32)
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import System.Posix.Pty (Pty, spawnWithPty, readPty, writePty, resizePty)
import System.Posix.User (getEffectiveUserID)
import System.Process (ProcessHandle, terminateProcess)

-- | Resize message from the client
data ResizeMessage = ResizeMessage
  { rmType :: !T.Text
  , rmCols :: !Int
  , rmRows :: !Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ResizeMessage where
  parseJSON = withObject "ResizeMessage" $ \o -> do
    t <- o .: "type"
    c <- o .: "cols"
    r <- o .: "rows"
    pure ResizeMessage { rmType = t, rmCols = c, rmRows = r }

-- | Determine the shell command and args based on effective UID.
-- When root (uid 0), runs bash via runuser as operator.
-- Otherwise, runs bash directly.
terminalShellCmd :: Word32 -> (String, [String])
terminalShellCmd uid
  | uid == 0  = ("runuser", ["-u", "operator", "--", "bash", "-l"])
  | otherwise = ("bash", ["-l"])

-- | Handle a terminal WebSocket connection
--
-- Spawns a PTY with a login shell as operator, bridges WS <-> PTY.
terminalHandler :: WS.Connection -> IO ()
terminalHandler conn = do
  -- Determine shell command based on whether we're root
  uid <- getEffectiveUserID
  let (shellCmd, shellArgs) = terminalShellCmd (fromIntegral uid)

  -- Spawn PTY with initial size 80x24
  (pty, ph) <- spawnWithPty
    Nothing           -- env (inherit)
    True              -- search PATH
    shellCmd          -- command
    shellArgs         -- args
    (80, 24)          -- initial size (cols, rows)

  -- Bridge PTY <-> WebSocket, cleanup on exit
  finally
    (race_ (ptyToWs pty conn) (wsToPty conn pty))
    (cleanup pty ph conn)

-- | Forward PTY output to WebSocket
ptyToWs :: Pty -> WS.Connection -> IO ()
ptyToWs pty conn = forever $ do
  chunk <- readPty pty
  catch
    (WS.sendBinaryData conn chunk)
    (\(_ :: SomeException) -> pure ())

-- | Forward WebSocket input to PTY (or handle resize)
wsToPty :: WS.Connection -> Pty -> IO ()
wsToPty conn pty = forever $ do
  msg <- WS.receiveData conn :: IO ByteString
  case Aeson.decodeStrict msg of
    Just (ResizeMessage { rmType = "resize", rmCols = cols, rmRows = rows }) ->
      resizePty pty (cols, rows)
    _ ->
      writePty pty msg

-- | Cleanup: kill PTY process and close WebSocket
cleanup :: Pty -> ProcessHandle -> WS.Connection -> IO ()
cleanup _pty ph conn = do
  -- Terminate the shell process
  catch (terminateProcess ph) (\(_ :: SomeException) -> pure ())
  -- Close WebSocket
  catch (WS.sendClose conn ("Terminal closed" :: T.Text)) (\(_ :: SomeException) -> pure ())
