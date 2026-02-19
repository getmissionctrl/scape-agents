{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Agent.TerminalSpec (spec) where

import Test.Hspec
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS

import Scape.Agent.Terminal (ResizeMessage(..), terminalShellCmd)

spec :: Spec
spec = describe "Terminal" $ do
  describe "ResizeMessage" $ do
    it "parses a valid resize message" $ do
      let json = "{\"type\":\"resize\",\"cols\":120,\"rows\":40}"
      let result = decode json :: Maybe ResizeMessage
      result `shouldSatisfy` \case
        Just msg -> rmType msg == "resize" && rmCols msg == 120 && rmRows msg == 40
        Nothing -> False

    it "rejects non-resize messages" $ do
      let json = "{\"type\":\"input\",\"data\":\"hello\"}"
      let result = decode json :: Maybe ResizeMessage
      -- Should parse but type won't be "resize"
      case result of
        Just msg -> rmType msg `shouldNotBe` "resize"
        Nothing -> pure ()  -- also acceptable

    it "rejects non-JSON input" $ do
      let raw = "ls -la\r\n"
      let result = decode (LBS.fromStrict raw) :: Maybe ResizeMessage
      result `shouldBe` Nothing

  describe "terminalShellCmd" $ do
    it "uses runuser as root" $ do
      let (cmd, args) = terminalShellCmd 0
      cmd `shouldBe` "runuser"
      args `shouldBe` ["-u", "operator", "--", "bash", "-l"]

    it "does not use /usr/bin/env" $ do
      let (cmd, _) = terminalShellCmd 0
      cmd `shouldNotBe` "/usr/bin/env"

    it "uses bash directly as non-root" $ do
      let (cmd, args) = terminalShellCmd 1000
      cmd `shouldBe` "bash"
      args `shouldBe` ["-l"]
