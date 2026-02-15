-- scape/test/Protocol/MMDSSpec.hs
{-# LANGUAGE OverloadedStrings #-}
module Protocol.MMDSSpec (spec) where

import Data.Aeson (decode, encode)
import Test.Hspec

import Scape.Protocol.MMDS

spec :: Spec
spec = describe "Protocol.MMDS" $ do
  describe "NatsConfig" $ do
    it "round-trips through JSON" $ do
      let cfg = NatsConfig
            { url = "nats://10.99.0.1:4222"
            , user = "agent-e9066c"
            , cmdSubject = "scape.cmd.e9066c"
            , obsPrefix = "scape.obs.e9066c"
            , announceSubject = Nothing
            , credsFileContent = Nothing
            }
      decode (encode cfg) `shouldBe` Just cfg

    it "round-trips announceSubject and credsFileContent through JSON" $ do
      let cfg = NatsConfig
            { url = "nats://10.99.0.1:4222"
            , user = "builder"
            , cmdSubject = "scape.cmd.builder"
            , obsPrefix = "scape.obs.builder"
            , announceSubject = Just "scape.build.mytemplate"
            , credsFileContent = Just "-----BEGIN NATS USER JWT-----\ntest-jwt\n------END NATS USER JWT------"
            }
      decode (encode cfg) `shouldBe` Just cfg

  describe "MMDSConfig" $ do
    it "round-trips through JSON" $ do
      let cfg = MMDSConfig
            { instanceId = "a1b2c3"
            , nats = NatsConfig
                { url = "nats://localhost:4222"
                , user = "agent-a1b2c3"
                , cmdSubject = "scape.cmd.a1b2c3"
                , obsPrefix = "scape.obs.a1b2c3"
                , announceSubject = Nothing
                , credsFileContent = Nothing
                }
            }
      decode (encode cfg) `shouldBe` Just cfg

  describe "MMDSData" $ do
    it "round-trips through JSON" $ do
      let mmds = MMDSData
            { scape = MMDSConfig
                { instanceId = "deadbeef"
                , nats = NatsConfig
                    { url = "nats://10.99.0.1:4222"
                    , user = "agent-deadbeef"
                    , cmdSubject = "scape.cmd.deadbeef"
                    , obsPrefix = "scape.obs.deadbeef"
                    , announceSubject = Nothing
                    , credsFileContent = Just "raw-creds-content"
                    }
                }
            }
      decode (encode mmds) `shouldBe` Just mmds

  describe "mkMMDSData" $ do
    it "constructs correct structure" $ do
      let mmds = mkMMDSData "abc123" "nats://host:4222" "myuser"
      instanceId (scape mmds) `shouldBe` "abc123"
      url (nats (scape mmds)) `shouldBe` "nats://host:4222"
      user (nats (scape mmds)) `shouldBe` "myuser"
      credsFileContent (nats (scape mmds)) `shouldBe` Nothing

    it "generates correct command subject" $ do
      let mmds = mkMMDSData "e9066c" "nats://localhost:4222" "u"
      cmdSubject (nats (scape mmds)) `shouldBe` "scape.cmd.e9066c"

    it "generates correct observation prefix" $ do
      let mmds = mkMMDSData "e9066c" "nats://localhost:4222" "u"
      obsPrefix (nats (scape mmds)) `shouldBe` "scape.obs.e9066c"

    it "uses instance ID in subject names" $ do
      let mmds1 = mkMMDSData "aaaaaa" "nats://x:1" "u"
      let mmds2 = mkMMDSData "bbbbbb" "nats://x:1" "u"
      cmdSubject (nats (scape mmds1)) `shouldBe` "scape.cmd.aaaaaa"
      cmdSubject (nats (scape mmds2)) `shouldBe` "scape.cmd.bbbbbb"
      obsPrefix (nats (scape mmds1)) `shouldBe` "scape.obs.aaaaaa"
      obsPrefix (nats (scape mmds2)) `shouldBe` "scape.obs.bbbbbb"

    it "sets announceSubject to Nothing by default" $ do
      let mmds = mkMMDSData "abc123" "nats://host:4222" "u"
      announceSubject (nats (scape mmds)) `shouldBe` Nothing
