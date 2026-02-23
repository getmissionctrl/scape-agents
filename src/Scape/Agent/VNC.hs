-- | VNC WebSocket proxy
--
-- Bridges a WebSocket connection to a local x11vnc TCP server,
-- enabling browser-based desktop viewing via noVNC.
--
-- Protocol: raw binary RFB frames in both directions.
-- The noVNC client in the browser speaks RFB natively over WebSocket.
module Scape.Agent.VNC
  ( vncHandler
  ) where

import Control.Concurrent.Async (race_)
import Control.Exception (SomeException, catch, finally, bracketOnError)
import Control.Monad (forever, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.WebSockets as WS

-- | VNC server address (x11vnc on localhost)
vncHost :: String
vncHost = "127.0.0.1"

vncPort :: String
vncPort = "5900"

-- | Handle a VNC WebSocket connection by bridging to local x11vnc.
vncHandler :: WS.Connection -> IO ()
vncHandler wsConn = do
  -- Resolve and connect to x11vnc
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
  addrs <- NS.getAddrInfo (Just hints) (Just vncHost) (Just vncPort)
  case addrs of
    [] -> do
      WS.sendClose wsConn ("VNC server not available" :: ByteString)
    (addr:_) -> do
      bracketOnError
        (NS.openSocket addr)
        NS.close
        $ \sock -> do
          NS.connect sock (NS.addrAddress addr)
          finally
            (race_ (tcpToWs sock wsConn) (wsToTcp wsConn sock))
            (cleanup sock wsConn)

-- | Forward TCP data from x11vnc to WebSocket
tcpToWs :: NS.Socket -> WS.Connection -> IO ()
tcpToWs sock wsConn = forever $ do
  bytes <- NSB.recv sock 65536
  when (BS.null bytes) $ ioError (userError "VNC server closed connection")
  catch
    (WS.sendBinaryData wsConn bytes)
    (\(_ :: SomeException) -> pure ())

-- | Forward WebSocket data to x11vnc TCP
wsToTcp :: WS.Connection -> NS.Socket -> IO ()
wsToTcp wsConn sock = forever $ do
  msg <- WS.receiveData wsConn :: IO ByteString
  NSB.sendAll sock msg

-- | Cleanup: close TCP socket and WebSocket
cleanup :: NS.Socket -> WS.Connection -> IO ()
cleanup sock wsConn = do
  catch (NS.close sock) (\(_ :: SomeException) -> pure ())
  catch (WS.sendClose wsConn ("VNC proxy closed" :: ByteString)) (\(_ :: SomeException) -> pure ())
