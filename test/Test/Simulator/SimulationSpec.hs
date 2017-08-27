module Test.Simulator.SimulationSpec where

import Control.Concurrent
import Control.Monad
import Data.List (nub, sort)
import Test.Hspec

import Lib
import Test.Common

import Andromeda.Simulator
import Andromeda.Assets.SpaceshipSample

makeRunningSimulation = do
  simModel <- compileSimModel networkDef
  pipe <- createPipe :: IO SimulatorPipe
  simHandle <- startSimulation pipe process simModel
  return (pipe, simHandle)

simulateSingleReq req = do
  (pipe, simHandle) <- makeRunningSimulation
  resp <- sendRequest pipe req
  stopSimulation simHandle
  return resp

spec = describe "Simulation tests" $ do
  it "Setting of value generator to unattached pipe should throw." $ do
    pipe <- createPipe :: IO SimulatorPipe
    sendRequest pipe (setGen1Act boostersNozzle1T) `shouldThrow` anyException

  it "Setting of value generator should be successfull." $
    simulateSingleReq (setGen1Act boostersNozzle1T) `shouldReturn` Ok

  it "Continuous simulation of sensor should return values." $ do
    (pipe, simHandle) <- makeRunningSimulation
    r1 <- sendRequest pipe (setGen1Act boostersNozzle1T)
    r2 <- sendRequest pipe runNetworkAct
    OutValueSource vs <- sendRequest pipe (GetValueSource boostersNozzle1T)
    vals <- replicateM 10 $ threadDelay 1000 >> readValueSource vs
    stopSimulation simHandle
    nub [r1, r2] `shouldBe` [Ok]
    sort vals `shouldBe` vals
    length vals `shouldBe` 10

  it "Continuous simulation of several sensors should return values." $ do
    (pipe, simHandle) <- makeRunningSimulation
    r1 <- sendRequest pipe (setGen1Act boostersNozzle1T)
    r2 <- sendRequest pipe (setGen2Act boostersNozzle2T)
    r3 <- sendRequest pipe runNetworkAct
    (OutValueSource vs1) <- sendRequest pipe (GetValueSource boostersNozzle1T)
    (OutValueSource vs2) <- sendRequest pipe (GetValueSource boostersNozzle2T)
    vals1 <- replicateM 10 $ threadDelay 1000 >> readValueSource vs1
    vals2 <- replicateM 10 $ threadDelay 1000 >> readValueSource vs2
    stopSimulation simHandle
    nub [r1, r2, r3] `shouldBe` [Ok]
    sort vals1 `shouldBe` vals1
    length vals1 `shouldBe` 10
    sort vals2 `shouldBe` reverse vals2
    length vals2 `shouldBe` 10
