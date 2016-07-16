module Simulator.SimulationSpec where

import Lib
import Andromeda
import TestCommon

import Test.Hspec

data In = Initialize
        | SetValueGenerator ComponentInstanceIndex ValueGenerator
        | Start ComponentInstanceIndex
        | Stop ComponentInstanceIndex
data Out = NoOut
         | Out String
  deriving (Show, Eq)
  
type SimulatorPipe = Pipe In Out

ok = Out "OK."

process :: Process In Out
process Initialize = return ok
process (SetValueGenerator idx g) = do
    setValueGenerator idx g
    return ok

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
    
spec = describe "Simulation test" $ do
    it "Initialization test should be initialized." $ do
        resp <- simulateSingleReq Initialize
        resp `shouldBe` ok
    it "SetValueGenerator test should generate values" $ do
        -- TODO: value generation
        resp <- simulateSingleReq (SetValueGenerator boostersNozzle1T floatIncrementGen)
        resp `shouldBe` ok
{--}