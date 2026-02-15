-- | Agent integration tests
--
-- Tests the full agent command execution flow:
-- 1. Agent connects to NATS
-- 2. Orchestrator sends command
-- 3. Agent executes and streams output
-- 4. Verify output encoding/decoding works correctly
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Integration.AgentSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import qualified Net.IPv4 as IPv4
import Test.Hspec

import qualified Scape.Nats as SN
import Scape.Nats (connectNats, disconnectNats, publishJson, subscribeJson)
import qualified Scape.Agent.Nats as AgentNats
import Scape.Agent.Nats (connectAgent, publishReady, publishOutputBytes, publishComplete)
import Scape.Protocol.Command (Command(..), ExecRequest(..))
import qualified Scape.Protocol.Command as Cmd
import Scape.Protocol.Observation (Observation(..), Channel(..))
import qualified Scape.Protocol.Observation as Obs

import Test.Nats (NatsTestEnv(..), withNatsServerSpec)

spec :: Spec
spec = withNatsServerSpec $ do
  describe "Agent Integration" $ do
    describe "NATS connectivity" $ do
      it "agent connects to NATS server" $ \env -> do
        let agentConfig = mkAgentConfig env "test-connect"
        (conn, _publisher) <- connectAgent agentConfig
        disconnectNats conn

      it "orchestrator connects to NATS server" $ \env -> do
        let orchConfig = mkOrchConfig env
        conn <- connectNats orchConfig
        disconnectNats conn

    describe "Agent observations" $ do
      it "agent publishes Ready, orchestrator receives it" $ \env -> do
        instId <- genInstanceId
        resultVar <- newEmptyMVar

        -- Orchestrator subscribes
        let orchConfig = mkOrchConfig env
        orchConn <- connectNats orchConfig
        let obsPattern = "scape.obs." <> instId <> ".ready"
        _ <- subscribeJson orchConn obsPattern $ \result ->
          putMVar resultVar result

        threadDelay 50000  -- 50ms for subscription to be active

        -- Agent connects and publishes Ready
        let agentConfig = mkAgentConfig env instId
        (agentConn, publisher) <- connectAgent agentConfig
        startTime <- getCurrentTime
        publishReady publisher instId "0.1.0" startTime (IPv4.ipv4 10 99 0 42)

        -- Wait for message
        received <- takeMVar resultVar
        disconnectNats agentConn
        disconnectNats orchConn

        case received of
          Right (ObsReady info) -> do
            Obs.instanceId info `shouldBe` instId
            Obs.version info `shouldBe` "0.1.0"
          Right other -> expectationFailure $ "Expected ObsReady, got: " <> show other
          Left err -> expectationFailure $ "Failed to decode: " <> err

      it "agent publishes Output (base64 encoded), orchestrator receives and decodes" $ \env -> do
        instId <- genInstanceId
        resultVar <- newEmptyMVar

        -- Orchestrator subscribes
        let orchConfig = mkOrchConfig env
        orchConn <- connectNats orchConfig
        let obsPattern = "scape.obs." <> instId <> ".output"
        _ <- subscribeJson orchConn obsPattern $ \result ->
          putMVar resultVar result

        threadDelay 50000

        -- Agent connects and publishes output (using publishOutputBytes which base64 encodes)
        let agentConfig = mkAgentConfig env instId
        (agentConn, publisher) <- connectAgent agentConfig

        cmdUuid <- nextRandom
        let cmdId = Cmd.CommandId cmdUuid
            -- This is what a real command would output
            rawOutput = "Hello World\nLine 2\n" :: BS.ByteString

        -- publishOutputBytes does the base64 encoding internally
        publishOutputBytes publisher cmdId Stdout rawOutput

        -- Receive and verify
        received <- takeMVar resultVar
        disconnectNats agentConn
        disconnectNats orchConn

        case received of
          Right (ObsOutput evt) -> do
            evt.commandId `shouldBe` cmdId
            evt.channel `shouldBe` Stdout
            -- The content should be base64 encoded
            let encodedContent = evt.content
            -- Decode it like the CLI does
            case B64.decode (TE.encodeUtf8 encodedContent) of
              Right decoded -> decoded `shouldBe` rawOutput
              Left err -> expectationFailure $ "Base64 decode failed: " <> err
          Right other -> expectationFailure $ "Expected ObsOutput, got: " <> show other
          Left err -> expectationFailure $ "Failed to decode: " <> err

      it "agent publishes Complete, orchestrator receives it" $ \env -> do
        instId <- genInstanceId
        resultVar <- newEmptyMVar

        let orchConfig = mkOrchConfig env
        orchConn <- connectNats orchConfig
        let obsPattern = "scape.obs." <> instId <> ".complete"
        _ <- subscribeJson orchConn obsPattern $ \result ->
          putMVar resultVar result

        threadDelay 50000

        let agentConfig = mkAgentConfig env instId
        (agentConn, publisher) <- connectAgent agentConfig

        cmdUuid <- nextRandom
        let cmdId = Cmd.CommandId cmdUuid
        publishComplete publisher cmdId 0 1.5

        received <- takeMVar resultVar
        disconnectNats agentConn
        disconnectNats orchConn

        case received of
          Right (ObsComplete evt) -> do
            evt.commandId `shouldBe` cmdId
            evt.exitCode `shouldBe` 0
            evt.duration `shouldBe` 1.5
          Right other -> expectationFailure $ "Expected ObsComplete, got: " <> show other
          Left err -> expectationFailure $ "Failed to decode: " <> err

    describe "Command flow" $ do
      it "orchestrator sends command, agent receives it" $ \env -> do
        instId <- genInstanceId
        resultVar <- newEmptyMVar

        -- Agent connects and subscribes to commands
        let agentConfig = mkAgentConfig env instId
        (agentConn, _) <- connectAgent agentConfig
        let cmdSubject = "scape.cmd." <> instId
        _ <- subscribeJson agentConn cmdSubject $ \result ->
          putMVar resultVar result

        threadDelay 50000

        -- Orchestrator connects and sends command
        let orchConfig = mkOrchConfig env
        orchConn <- connectNats orchConfig

        cmdUuid <- nextRandom
        let cmdId = Cmd.CommandId cmdUuid
            execReq = ExecRequest
              { Cmd.id = cmdId
              , Cmd.command = "echo"
              , Cmd.args = ["hello", "world"]
              , Cmd.env = Map.empty
              , Cmd.workdir = "/tmp"
              , Cmd.stdin = Nothing
              , Cmd.timeout = 30
              }
            cmd = CmdExec execReq

        publishJson orchConn cmdSubject cmd

        received <- takeMVar resultVar
        disconnectNats agentConn
        disconnectNats orchConn

        case received of
          Right (CmdExec req) -> do
            req.id `shouldBe` cmdId
            req.command `shouldBe` "echo"
            req.args `shouldBe` ["hello", "world"]
          Right other -> expectationFailure $ "Expected CmdExec, got: " <> show other
          Left err -> expectationFailure $ "Failed to decode: " <> err

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Create orchestrator config for test NATS server (no auth)
mkOrchConfig :: NatsTestEnv -> SN.NatsConfig
mkOrchConfig env = SN.NatsConfig
  { SN.url = env.natsUrl
  , SN.user = Nothing
  , SN.password = Nothing
  , SN.credsFile = Nothing
  }

-- | Create agent config for test NATS server (no auth)
mkAgentConfig :: NatsTestEnv -> Text -> AgentNats.NatsAgentConfig
mkAgentConfig env instId = AgentNats.NatsAgentConfig
  instId                         -- instanceId
  env.natsUrl                    -- natsUrl
  ""                             -- natsUser
  ("scape.cmd." <> instId)       -- cmdSubject
  ("scape.obs." <> instId)       -- obsPrefix
  Nothing                        -- credsFile
  Nothing                        -- credsFileContent
  Nothing                        -- announceSubject

-- | Generate a unique instance ID
genInstanceId :: IO Text
genInstanceId = do
  uuid <- nextRandom
  pure $ T.take 8 $ UUID.toText uuid
