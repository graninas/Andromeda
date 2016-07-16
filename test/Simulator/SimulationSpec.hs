module Simulator.SimulationSpec where

import Lib
import Andromeda
import TestCommon

import Test.Hspec

data In = Initialize
        | SimStateAction (SimState ())
        | Start ComponentInstanceIndex
        | Stop ComponentInstanceIndex
data Out = NoOut
         | Out String
  deriving (Show, Eq)
  
type SimulatorPipe = Pipe In Out

ok = Out "OK."

process :: Process In Out
process Initialize = return ok
process (SimStateAction act) = do
    act
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
    
action = SimStateAction
    
spec = describe "Simulation test" $ do
    it "Initialization should be successfull." $ do
        resp <- simulateSingleReq Initialize
        resp `shouldBe` ok
    it "Setting of value generator should be successfull." $ do
        -- TODO: value generation
        resp <- simulateSingleReq (action $ setValueGenerator boostersNozzle1T floatIncrementGen)
        resp `shouldBe` ok
