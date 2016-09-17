module Simulator.SimulationSpec where

import Lib
import Andromeda
import TestCommon
import Simulator.TestCommon

import Test.Hspec
import Data.List (nub, sort)
import Control.Concurrent


spec = describe "Simulation test" $ do
    it "Initialization should be successfull." $
        simulateSingleReq Initialize `shouldReturn` ok
        
    it "Setting of value generator to unattached pipe should throw." $ do
        pipe <- createPipe :: IO SimulatorPipe
        sendRequest pipe (setGen1Act boostersNozzle1T) `shouldThrow` anyException
        
    it "Setting of value generator should be successfull." $
        simulateSingleReq (setGen1Act boostersNozzle1T) `shouldReturn` ok
        
    it "Continuous simulation of sensor should return values." $ do
        (pipe, simHandle) <- makeRunningSimulation
        r1 <- sendRequest pipe (setGen1Act boostersNozzle1T)
        r2 <- sendRequest pipe runNetworkAct
        (OutValueSource vs) <- sendRequest pipe (GetValueSource boostersNozzle1T)
        vals <- sequence (replicate 10 $ threadDelay 1000 >> readValueSource vs)
        stopSimulation simHandle
        nub [r1, r2] `shouldBe` [ok]
        sort vals `shouldBe` vals
        length vals `shouldBe` 10
        
    it "Continuous simulation of several sensors should return values." $ do
        (pipe, simHandle) <- makeRunningSimulation
        r1 <- sendRequest pipe (setGen1Act boostersNozzle1T)
        r2 <- sendRequest pipe (setGen2Act boostersNozzle2T)
        r3 <- sendRequest pipe runNetworkAct
        (OutValueSource vs1) <- sendRequest pipe (GetValueSource boostersNozzle1T)
        (OutValueSource vs2) <- sendRequest pipe (GetValueSource boostersNozzle2T)
        vals1 <- sequence (replicate 10 $ threadDelay 1000 >> readValueSource vs1)
        vals2 <- sequence (replicate 10 $ threadDelay 1000 >> readValueSource vs2)
        stopSimulation simHandle
        nub [r1, r2, r3] `shouldBe` [ok]
        sort vals1 `shouldBe` vals1
        length vals1 `shouldBe` 10
        sort vals2 `shouldBe` (reverse vals2)
        length vals2 `shouldBe` 10