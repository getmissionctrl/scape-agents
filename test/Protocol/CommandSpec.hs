-- scape/test/Protocol/CommandSpec.hs
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol.CommandSpec (spec) where

import Data.Aeson (decode, encode)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Scape.Protocol.Command

spec :: Spec
spec = describe "Protocol.Command" $ do
  describe "CommandId" $ do
    it "round-trips through JSON" $ do
      cmdId <- newCommandId
      decode (encode cmdId) `shouldBe` Just cmdId

  describe "ExecRequest" $ do
    it "round-trips through JSON with all fields" $ do
      cmdId <- newCommandId
      let req = ExecRequest
            { id = cmdId
            , command = "python"
            , args = ["-c", "print('hello')"]
            , env = Map.fromList [("PATH", "/usr/bin"), ("HOME", "/root")]
            , workdir = "/tmp"
            , stdin = Just "aW5wdXQ="  -- base64 "input"
            , timeout = 30
            }
      decode (encode req) `shouldBe` Just req

    it "round-trips with empty optional fields" $ do
      cmdId <- newCommandId
      let req = ExecRequest
            { id = cmdId
            , command = "ls"
            , args = []
            , env = Map.empty
            , workdir = "/"
            , stdin = Nothing
            , timeout = 60
            }
      decode (encode req) `shouldBe` Just req

  describe "WriteFileRequest" $ do
    it "round-trips through JSON" $ do
      let req = WriteFileRequest
            { path = "/tmp/test.txt"
            , content = "aGVsbG8gd29ybGQ="  -- base64 "hello world"
            , mode = Just 0o644
            }
      decode (encode req) `shouldBe` Just req

    it "round-trips with no mode" $ do
      let req = WriteFileRequest
            { path = "/etc/config"
            , content = "Y29uZmlnCg=="
            , mode = Nothing
            }
      decode (encode req) `shouldBe` Just req

  describe "ReadFileRequest" $ do
    it "round-trips through JSON" $ do
      let req = ReadFileRequest
            { path = "/var/log/syslog"
            , maxBytes = Just 4096
            }
      decode (encode req) `shouldBe` Just req

    it "round-trips with no limit" $ do
      let req = ReadFileRequest
            { path = "/etc/passwd"
            , maxBytes = Nothing
            }
      decode (encode req) `shouldBe` Just req

  describe "SecretsRequest" $ do
    it "round-trips through JSON" $ do
      let req = SecretsRequest
            { secrets = Map.fromList [("API_KEY", "secret123"), ("DB_PASS", "hunter2")]
            , targetDir = "/run/scape/secrets"
            }
      decode (encode req) `shouldBe` Just req

    it "round-trips with empty secrets" $ do
      let req = SecretsRequest
            { secrets = Map.empty
            , targetDir = "/tmp/secrets"
            }
      decode (encode req) `shouldBe` Just req

  describe "Command" $ do
    it "round-trips CmdExec" $ do
      cmdId <- newCommandId
      let cmd = CmdExec ExecRequest
            { id = cmdId
            , command = "echo"
            , args = ["hello"]
            , env = Map.empty
            , workdir = "/home"
            , stdin = Nothing
            , timeout = 10
            }
      decode (encode cmd) `shouldBe` Just cmd

    it "round-trips CmdCancel" $ do
      cmdId <- newCommandId
      let cmd = CmdCancel cmdId
      decode (encode cmd) `shouldBe` Just cmd

    it "round-trips CmdWriteFile" $ do
      let cmd = CmdWriteFile WriteFileRequest
            { path = "/tmp/file"
            , content = "dGVzdA=="
            , mode = Nothing
            }
      decode (encode cmd) `shouldBe` Just cmd

    it "round-trips CmdReadFile" $ do
      let cmd = CmdReadFile ReadFileRequest
            { path = "/tmp/file"
            , maxBytes = Just 1024
            }
      decode (encode cmd) `shouldBe` Just cmd

    it "round-trips CmdInjectSecrets" $ do
      let cmd = CmdInjectSecrets SecretsRequest
            { secrets = Map.singleton "KEY" "value"
            , targetDir = "/run/secrets"
            }
      decode (encode cmd) `shouldBe` Just cmd

    it "round-trips CmdPing" $ do
      decode (encode CmdPing) `shouldBe` Just CmdPing

    it "round-trips CmdShutdown" $ do
      decode (encode CmdShutdown) `shouldBe` Just CmdShutdown
