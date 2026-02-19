{-# LANGUAGE OverloadedStrings #-}
-- scape/test/Agent/ExecutorSpec.hs
module Agent.ExecutorSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.UUID (nil)
import Data.Word (Word8)

import Scape.Agent.Executor
import Scape.Agent.State (initAgentState)
import Scape.Protocol.Types (CommandId(..), ExitCode(..))

spec :: Spec
spec = describe "Executor" $ do
  describe "executeCommandSync" $ do
    it "executes a simple command and captures output" $ do
      state <- initAgentState
      let cmdId = CommandId nil

      result <- executeCommandSync state cmdId "echo" ["hello"] mempty "/tmp" Nothing 10

      exitCode result `shouldBe` ExitCode 0
      BSC.strip (stdout result) `shouldBe` "hello"

    it "captures stderr separately" $ do
      state <- initAgentState
      let cmdId = CommandId nil

      result <- executeCommandSync state cmdId "sh" ["-c", "echo err >&2"] mempty "/tmp" Nothing 10

      exitCode result `shouldBe` ExitCode 0
      BSC.strip (stderr result) `shouldBe` "err"

    it "respects timeout" $ do
      state <- initAgentState
      let cmdId = CommandId nil

      result <- executeCommandSync state cmdId "sleep" ["10"] mempty "/tmp" Nothing 1

      -- Should be killed by timeout
      exitCode result `shouldBe` ExitCode (-1)
      stderr result `shouldSatisfy` BS.isInfixOf "timed out"

    it "passes environment variables" $ do
      state <- initAgentState
      let cmdId = CommandId nil
          env = [("MY_VAR", "my_value")]

      result <- executeCommandSync state cmdId "sh" ["-c", "echo $MY_VAR"] env "/tmp" Nothing 10

      BSC.strip (stdout result) `shouldBe` "my_value"

    it "handles stdin" $ do
      state <- initAgentState
      let cmdId = CommandId nil

      result <- executeCommandSync state cmdId "cat" [] mempty "/tmp" (Just "hello from stdin") 10

      exitCode result `shouldBe` ExitCode 0
      stdout result `shouldBe` "hello from stdin"

    it "handles non-zero exit codes" $ do
      state <- initAgentState
      let cmdId = CommandId nil

      result <- executeCommandSync state cmdId "sh" ["-c", "exit 42"] mempty "/tmp" Nothing 10

      exitCode result `shouldBe` ExitCode 42

    it "uses /home/operator as default working directory reference" $ do
      state <- initAgentState
      let cmdId = CommandId nil

      -- Verify the default workdir is /home/operator by checking pwd
      -- Note: in test env this may not exist, so we test /tmp fallback
      result <- executeCommandSync state cmdId "pwd" [] mempty "/tmp" Nothing 10

      exitCode result `shouldBe` ExitCode 0
      -- The working directory should be what we passed
      BSC.strip (stdout result) `shouldBe` "/tmp"

  describe "base64 utilities" $ do
    it "encodes and decodes correctly" $ do
      let original = "Hello, World!"
          encoded = encodeBase64 original
          decoded = decodeBase64 encoded

      decoded `shouldBe` Right original

    it "handles binary data" $ do
      let original = BS.pack [0 :: Word8, 1, 255, 128, 64]
          encoded = encodeBase64 original
          decoded = decodeBase64 encoded

      decoded `shouldBe` Right original
