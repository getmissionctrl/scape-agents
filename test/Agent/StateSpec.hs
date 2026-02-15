{-# LANGUAGE OverloadedStrings #-}
-- scape/test/Agent/StateSpec.hs
module Agent.StateSpec (spec) where

import Test.Hspec
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map

import Scape.Agent.State

spec :: Spec
spec = describe "AgentState" $ do
  it "initializes with empty state" $ do
    state <- initAgentState
    secrets <- readTVarIO (secrets state)
    secrets `shouldBe` Map.empty

  it "can inject and retrieve secrets" $ do
    state <- initAgentState
    injectSecret state "API_KEY" "secret123"
    secrets <- readTVarIO (secrets state)
    Map.lookup "API_KEY" secrets `shouldBe` Just "secret123"

  it "can clear secrets" $ do
    state <- initAgentState
    injectSecret state "API_KEY" "secret123"
    clearSecrets state
    secrets <- readTVarIO (secrets state)
    secrets `shouldBe` Map.empty
