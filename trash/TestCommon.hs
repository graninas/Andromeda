module Test.Simulator.TestCommon where

import Andromeda.Assets.SpaceshipSample
import Andromeda.Simulator

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

runNetworkAct = SimAction $ runNetwork
setGen1Act idx = SimAction $ setValueGenerator idx floatIncrementGen
setGen2Act idx = SimAction $ setValueGenerator idx floatDecrementGen
