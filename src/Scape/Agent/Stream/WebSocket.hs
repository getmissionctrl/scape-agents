-- scape/src/Scape/Agent/Stream/WebSocket.hs
module Scape.Agent.Stream.WebSocket
  ( wsHandler
  ) where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception (finally, catch, SomeException)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Network.WebSockets as WS

import Scape.Agent.State
import Scape.Protocol.Types (CommandId, Token(..))

-- | Handle a WebSocket connection for command streaming
--
-- This handler provides bidirectional communication:
-- - Output from the running command is sent to the WebSocket client
-- - Input from the WebSocket client is forwarded to the command's stdin
wsHandler
  :: AgentState
  -> CommandId
  -> Maybe Token
  -> WS.Connection
  -> IO ()
wsHandler state cmdId mToken conn = do
  -- Validate token
  mCmd <- getCommand state cmdId
  case mCmd of
    Nothing -> do
      WS.sendClose conn ("Command not found" :: Text)
    Just cmdState -> do
      let validToken = token cmdState
      case mToken of
        Nothing -> WS.sendClose conn ("Missing token" :: Text)
        Just tkn
          | tkn /= validToken ->
              WS.sendClose conn ("Invalid token" :: Text)
          | otherwise -> runStream state cmdId cmdState conn

-- | Run the bidirectional stream
runStream :: AgentState -> CommandId -> CommandState -> WS.Connection -> IO ()
runStream state cmdId cmdState conn = do
  -- Create output channel for this subscriber
  outChan <- newTChanIO
  let subscriber = WSSubscriber outChan

  -- Register subscriber
  addSubscriber state cmdId subscriber

  -- Run bidirectional forwarding, cleanup on exit
  finally
    (race_ (forwardOutput outChan) (forwardInput (stdinChan cmdState)))
    (removeSubscriber state cmdId subscriber)
  where
    -- Forward output from command to WebSocket
    forwardOutput :: TChan ByteString -> IO ()
    forwardOutput outCh = forever $ do
      chunk <- atomically $ readTChan outCh
      catch
        (WS.sendBinaryData conn chunk)
        (\(_ :: SomeException) -> pure ())  -- Client disconnected

    -- Forward input from WebSocket to command stdin
    forwardInput :: TChan ByteString -> IO ()
    forwardInput stdinCh = forever $ do
      msg <- WS.receiveData conn
      atomically $ writeTChan stdinCh msg
