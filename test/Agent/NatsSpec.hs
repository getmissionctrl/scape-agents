-- scape/test/Agent/NatsSpec.hs
{-# LANGUAGE OverloadedStrings #-}
module Agent.NatsSpec (spec) where

import Data.Aeson (encode, eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Net.IPv4 as IPv4
import Test.Hspec

import Scape.Agent.Nats (NatsAgentConfig(..), fromMMDSConfig, discoverIpAddress)
import qualified Scape.Agent.Nats as AN
import qualified Scape.Protocol.MMDS as MMDS

spec :: Spec
spec = describe "Nats" $ do
  describe "NatsAgentConfig" $ do
    it "constructs with all fields" $ do
      let cfg = NatsAgentConfig
            { instanceId = "test-123"
            , natsUrl = "nats://localhost:4222"
            , natsUser = "agent-test-123"
            , cmdSubject = "scape.cmd.test-123"
            , obsPrefix = "scape.obs.test-123"
            , credsFile = Nothing
            , credsFileContent = Nothing
            , announceSubject = Nothing
            }
      instanceId cfg `shouldBe` "test-123"
      natsUrl cfg `shouldBe` "nats://localhost:4222"
      natsUser cfg `shouldBe` "agent-test-123"
      cmdSubject cfg `shouldBe` "scape.cmd.test-123"
      obsPrefix cfg `shouldBe` "scape.obs.test-123"
      announceSubject cfg `shouldBe` Nothing
      credsFileContent cfg `shouldBe` Nothing

    it "supports equality comparison" $ do
      let cfg1 = NatsAgentConfig "a" "url" "user" "cmd" "obs" Nothing Nothing Nothing
          cfg2 = NatsAgentConfig "a" "url" "user" "cmd" "obs" Nothing Nothing Nothing
          cfg3 = NatsAgentConfig "b" "url" "user" "cmd" "obs" Nothing Nothing Nothing
      cfg1 `shouldBe` cfg2
      cfg1 `shouldNotBe` cfg3

    it "supports announceSubject for template builds" $ do
      let cfg = NatsAgentConfig "tpl-test" "url" "builder" "cmd" "obs" Nothing Nothing (Just "scape.build.mytemplate")
      announceSubject cfg `shouldBe` Just "scape.build.mytemplate"

    it "supports credsFileContent for MMDS-based auth" $ do
      let credsContent = "-----BEGIN NATS USER JWT-----\ntest-jwt\n------END NATS USER JWT------"
          cfg = NatsAgentConfig "tpl-test" "url" "builder" "cmd" "obs" Nothing (Just credsContent) Nothing
      credsFileContent cfg `shouldBe` Just credsContent

  describe "fromMMDSConfig" $ do
    it "converts MMDS NatsConfig to NatsAgentConfig" $ do
      let mmdsNats = MMDS.NatsConfig
            { MMDS.url = "nats://10.99.0.1:4222"
            , MMDS.user = "agent-abc123"
            , MMDS.cmdSubject = "scape.cmd.abc123"
            , MMDS.obsPrefix = "scape.obs.abc123"
            , MMDS.announceSubject = Nothing
            , MMDS.credsFileContent = Nothing
            }
      let result = fromMMDSConfig "abc123" mmdsNats
      instanceId result `shouldBe` "abc123"
      natsUrl result `shouldBe` "nats://10.99.0.1:4222"
      natsUser result `shouldBe` "agent-abc123"
      cmdSubject result `shouldBe` "scape.cmd.abc123"
      obsPrefix result `shouldBe` "scape.obs.abc123"
      credsFile result `shouldBe` Nothing  -- Used for local testing only
      credsFileContent result `shouldBe` Nothing
      announceSubject result `shouldBe` Nothing

    it "preserves special characters in instance ID" $ do
      let mmdsNats = MMDS.NatsConfig
            { MMDS.url = "nats://localhost:4222"
            , MMDS.user = "agent"
            , MMDS.cmdSubject = "scape.cmd.e9066c-ab"
            , MMDS.obsPrefix = "scape.obs.e9066c-ab"
            , MMDS.announceSubject = Nothing
            , MMDS.credsFileContent = Nothing
            }
      let result = fromMMDSConfig "e9066c-ab" mmdsNats
      instanceId result `shouldBe` "e9066c-ab"

    it "passes through announceSubject and credsFileContent from MMDS" $ do
      let credsContent = "-----BEGIN NATS USER JWT-----\ntest-jwt\n------END NATS USER JWT------"
          mmdsNats = MMDS.NatsConfig
            { MMDS.url = "nats://10.99.0.1:4222"
            , MMDS.user = "builder"
            , MMDS.cmdSubject = "scape.cmd.builder"
            , MMDS.obsPrefix = "scape.build.mytemplate"
            , MMDS.announceSubject = Just "scape.build.mytemplate.ready"
            , MMDS.credsFileContent = Just credsContent
            }
      let result = fromMMDSConfig "mytemplate" mmdsNats
      announceSubject result `shouldBe` Just "scape.build.mytemplate.ready"
      credsFileContent result `shouldBe` Just credsContent

  describe "ReadyAnnouncement" $ do
    it "round-trips through JSON" $ do
      -- ReadyAnnouncement fields: instanceId, ipAddress, port, ready
      let ann = AN.ReadyAnnouncement "python-sandbox" "10.99.0.42" 8080 True
      eitherDecode (encode ann) `shouldBe` Right ann

    it "encodes with expected field names" $ do
      let ann = AN.ReadyAnnouncement "test" "10.0.0.1" 9090 True
          encoded = encode ann
          decoded = eitherDecode encoded :: Either String Aeson.Value
      case decoded of
        Left err -> expectationFailure err
        Right (Aeson.Object obj) -> do
          KM.lookup "ip" obj `shouldBe` Just (Aeson.String "10.0.0.1")
          KM.lookup "port" obj `shouldBe` Just (Aeson.Number 9090)
          KM.lookup "ready" obj `shouldBe` Just (Aeson.Bool True)
          KM.lookup "instanceId" obj `shouldBe` Just (Aeson.String "test")
        Right _ -> expectationFailure "Expected JSON object"

  describe "discoverIpAddress" $ do
    it "discovers a valid IPv4 address on the host machine" $ do
      -- This test runs on the host, which should have at least one private IP
      -- The function now falls back to loopback if no private IP is found
      ip <- discoverIpAddress
      -- IPv4 type guarantees valid format
      -- Check it's not 0.0.0.0
      ip `shouldNotBe` IPv4.any
      -- If it's loopback (127.0.0.1), that means no private IP was found
      -- but that's acceptable in sandboxed environments
      pure ()

    it "returns consistent results on repeated calls" $ do
      ip1 <- discoverIpAddress
      ip2 <- discoverIpAddress
      -- Results should be deterministic
      ip1 `shouldBe` ip2
