module Main where

import Test.Hspec
import qualified Agent.StateSpec
import qualified Agent.ExecutorSpec
import qualified Agent.NatsSpec
import qualified Agent.TerminalSpec
import qualified Integration.AgentSpec
import qualified Protocol.CommandSpec
import qualified Protocol.ObservationSpec
import qualified Protocol.MMDSSpec

main :: IO ()
main = hspec $ do
  describe "Agent" $ do
    describe "State" Agent.StateSpec.spec
    describe "Executor" Agent.ExecutorSpec.spec
    describe "Nats" Agent.NatsSpec.spec
    describe "Terminal" Agent.TerminalSpec.spec
  describe "Protocol" $ do
    describe "Command" Protocol.CommandSpec.spec
    describe "Observation" Protocol.ObservationSpec.spec
    describe "MMDS" Protocol.MMDSSpec.spec
  -- Integration tests
  describe "Integration" $ do
    -- Agent tests (spins up its own NATS server)
    Integration.AgentSpec.spec
