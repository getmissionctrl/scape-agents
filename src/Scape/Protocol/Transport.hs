-- scape/src/Scape/Protocol/Transport.hs
--
-- Shared transport utilities for length-prefixed JSON protocol.
-- Used by both agent (Scape.Agent.Vsock) and orchestrator/ctl.
module Scape.Protocol.Transport
  ( -- * Sending
    sendMessage
  , sendCommand
  , sendEvent

    -- * Receiving
  , recvMessage
  , recvExactly
  , decodeLen
  , encodeLen

    -- * Connection helpers
  , connectTcp
  , AcceptResult(..)
  , acceptConnection
  ) where

import Control.Exception (catch, SomeException)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)

import Scape.Protocol.Control (ControlCommand, ControlEvent)

-- | Send any JSON-serializable message as length-prefixed data
sendMessage :: ToJSON a => Socket -> a -> IO ()
sendMessage sock msg = do
  let msgBs = encode msg
      lenBs = encodeLen (LBS.length msgBs)
  sendAll sock (lenBs <> msgBs)

-- | Send a command (convenience wrapper)
sendCommand :: Socket -> ControlCommand -> IO ()
sendCommand = sendMessage

-- | Send an event (convenience wrapper)
sendEvent :: Socket -> ControlEvent -> IO ()
sendEvent = sendMessage

-- | Receive a length-prefixed JSON message
recvMessage :: FromJSON a => Socket -> IO (Maybe a)
recvMessage sock = do
  lenBs <- recvExactly sock 4
  if LBS.null lenBs
    then pure Nothing  -- Connection closed
    else do
      let len = fromIntegral $ decodeLen lenBs
      msgBs <- recvExactly sock len
      pure $ decode msgBs

-- | Receive exactly n bytes from socket (handles partial reads)
recvExactly :: Socket -> Int -> IO LBS.ByteString
recvExactly sock n = go n []
  where
    go 0 chunks = pure $ LBS.concat (reverse chunks)
    go remaining chunks = do
      chunk <- recv sock (fromIntegral remaining)
      if LBS.null chunk
        then pure LBS.empty  -- Connection closed
        else go (remaining - fromIntegral (LBS.length chunk)) (chunk : chunks)

-- | Encode length as 4-byte little-endian
encodeLen :: Integral a => a -> LBS.ByteString
encodeLen len = LBS.pack
  [ fromIntegral (len `mod` 256)
  , fromIntegral ((len `div` 256) `mod` 256)
  , fromIntegral ((len `div` 65536) `mod` 256)
  , fromIntegral ((len `div` 16777216) `mod` 256)
  ]

-- | Decode 4-byte little-endian length
decodeLen :: LBS.ByteString -> Word32
decodeLen bs = case LBS.unpack bs of
  [a, b, c, d] -> fromIntegral a
                + fromIntegral b * 256
                + fromIntegral c * 65536
                + fromIntegral d * 16777216
  _ -> 0

-- | Connect to a TCP endpoint
connectTcp :: Text -> Int -> IO (Either Text Socket)
connectTcp host port = catch doConnect handleError
  where
    doConnect = do
      let hints = defaultHints { addrSocketType = Stream }
      addrInfos <- getAddrInfo (Just hints) (Just $ T.unpack host) (Just $ show port)
      case addrInfos of
        [] -> pure $ Left $ "Cannot resolve: " <> host
        (addrInfo:_) -> do
          sock <- openSocket addrInfo
          connect sock (addrAddress addrInfo)
          pure $ Right sock

    handleError :: SomeException -> IO (Either Text Socket)
    handleError e = pure $ Left $ T.pack $ show e

-- | Result of accepting a connection
data AcceptResult = AcceptResult
  { arSocket  :: !Socket
  , arAddress :: !SockAddr
  }

-- | Accept a connection on a listening socket
acceptConnection :: Socket -> IO AcceptResult
acceptConnection serverSock = do
  (clientSock, clientAddr) <- accept serverSock
  pure $ AcceptResult clientSock clientAddr
