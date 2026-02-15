-- scape/test/Protocol/ObservationSpec.hs
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol.ObservationSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Time (getCurrentTime)
import qualified Net.IPv4 as IPv4
import Test.Hspec

import Scape.Protocol.Command (newCommandId)
import Scape.Protocol.Observation

spec :: Spec
spec = describe "Protocol.Observation" $ do
  describe "Channel" $ do
    it "round-trips Stdout" $ do
      decode (encode Stdout) `shouldBe` Just Stdout

    it "round-trips Stderr" $ do
      decode (encode Stderr) `shouldBe` Just Stderr

  describe "AgentInfo" $ do
    it "round-trips through JSON" $ do
      now <- getCurrentTime
      let info = AgentInfo
            { instanceId = "a1b2c3"
            , version = "0.1.0"
            , startedAt = now
            , ipAddress = IPv4.ipv4 10 99 0 42
            }
      decode (encode info) `shouldBe` Just info

  describe "OutputEvent" $ do
    it "round-trips stdout event" $ do
      cmdId <- newCommandId
      let evt = OutputEvent
            { commandId = cmdId
            , channel = Stdout
            , content = "aGVsbG8gd29ybGQK"  -- base64 "hello world\n"
            }
      decode (encode evt) `shouldBe` Just evt

    it "round-trips stderr event" $ do
      cmdId <- newCommandId
      let evt = OutputEvent
            { commandId = cmdId
            , channel = Stderr
            , content = "ZXJyb3I6IGZhaWxlZAo="  -- base64 "error: failed\n"
            }
      decode (encode evt) `shouldBe` Just evt

  describe "CompleteEvent" $ do
    it "round-trips successful completion" $ do
      cmdId <- newCommandId
      let evt = CompleteEvent
            { commandId = cmdId
            , exitCode = 0
            , duration = 1.234
            }
      decode (encode evt) `shouldBe` Just evt

    it "round-trips failed completion" $ do
      cmdId <- newCommandId
      let evt = CompleteEvent
            { commandId = cmdId
            , exitCode = 1
            , duration = 0.5
            }
      decode (encode evt) `shouldBe` Just evt

  describe "ErrorEvent" $ do
    it "round-trips command error" $ do
      cmdId <- newCommandId
      let evt = ErrorEvent
            { commandId = Just cmdId
            , message = "Command timed out"
            }
      decode (encode evt) `shouldBe` Just evt

    it "round-trips agent-level error" $ do
      let evt = ErrorEvent
            { commandId = Nothing
            , message = "Out of memory"
            }
      decode (encode evt) `shouldBe` Just evt

  describe "MetricsEvent" $ do
    it "round-trips through JSON" $ do
      let evt = MetricsEvent
            { commandsTotal = 100
            , commandsActive = 3
            , memoryUsedKb = 524288
            , cpuPercent = 45.5
            }
      decode (encode evt) `shouldBe` Just evt

  describe "Observation" $ do
    it "round-trips ObsReady" $ do
      now <- getCurrentTime
      let obs = ObsReady AgentInfo
            { instanceId = "e9066c"
            , version = "1.0.0"
            , startedAt = now
            , ipAddress = IPv4.ipv4 10 99 0 100
            }
      decode (encode obs) `shouldBe` Just obs

    it "round-trips ObsOutput" $ do
      cmdId <- newCommandId
      let obs = ObsOutput OutputEvent
            { commandId = cmdId
            , channel = Stdout
            , content = "dGVzdA=="
            }
      decode (encode obs) `shouldBe` Just obs

    it "round-trips ObsComplete" $ do
      cmdId <- newCommandId
      let obs = ObsComplete CompleteEvent
            { commandId = cmdId
            , exitCode = 0
            , duration = 2.5
            }
      decode (encode obs) `shouldBe` Just obs

    it "round-trips ObsError" $ do
      let obs = ObsError ErrorEvent
            { commandId = Nothing
            , message = "Fatal error"
            }
      decode (encode obs) `shouldBe` Just obs

    it "round-trips ObsMetrics" $ do
      let obs = ObsMetrics MetricsEvent
            { commandsTotal = 50
            , commandsActive = 1
            , memoryUsedKb = 262144
            , cpuPercent = 12.3
            }
      decode (encode obs) `shouldBe` Just obs

    it "round-trips ObsShuttingDown" $ do
      decode (encode ObsShuttingDown) `shouldBe` Just ObsShuttingDown

    it "round-trips ObsFileContent" $ do
      let obs = ObsFileContent FileContentEvent
            { path = "/tmp/test.txt"
            , content = "aGVsbG8="  -- base64 "hello"
            , size = 5
            }
      decode (encode obs) `shouldBe` Just obs

    it "round-trips ObsFileWritten" $ do
      let obs = ObsFileWritten FileWrittenEvent
            { path = "/tmp/test.txt"
            , size = 5
            }
      decode (encode obs) `shouldBe` Just obs

    it "round-trips ObsSecretsInjected" $ do
      let obs = ObsSecretsInjected SecretsInjectedEvent
            { count = 3
            , targetDir = "/run/scape/secrets"
            }
      decode (encode obs) `shouldBe` Just obs

  describe "FileContentEvent" $ do
    it "roundtrips through JSON" $ do
      let ev = FileContentEvent "/tmp/test.txt" "aGVsbG8=" 5
      decode (encode ev) `shouldBe` Just ev

  describe "FileWrittenEvent" $ do
    it "roundtrips through JSON" $ do
      let ev = FileWrittenEvent "/tmp/test.txt" 5
      decode (encode ev) `shouldBe` Just ev

  describe "SecretsInjectedEvent" $ do
    it "roundtrips through JSON" $ do
      let ev = SecretsInjectedEvent 3 "/run/scape/secrets"
      decode (encode ev) `shouldBe` Just ev
