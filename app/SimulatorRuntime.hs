module SimulatorRuntime where

import Andromeda
import Lib

import Assets.SpaceshipSample

import Control.Concurrent.MVar
import Control.Monad (when)
import Data.List as L (nub)

type SimulatorPipe = Pipe In Out

-- Can type families be used here?

data In = SimAction (SimState ())
        | GetDevices
        | GetValueSource ComponentInstanceIndex
        | Start ComponentInstanceIndex
        | Stop ComponentInstanceIndex

data Out = Ok
         | OutValueSource ValueSource
         | OutDevices { outDevices :: [Device] }
         
instance Eq Out where
    Ok == Ok = True
    _ == _ = False

-- dummy
getDeviceDefs = return
    [ makeDevice boostersDef "boosters"
    , makeDevice rotaryEngineDef "rotary engine" ]
    
process :: Process In Out
process (SimAction act) = act >> return Ok
process GetDevices = getDeviceDefs >>= (return . OutDevices)
process (GetValueSource idx) = getValueSource idx >>= (return . OutValueSource)

runNetworkAct = SimAction $ runNetwork
setGen1Act idx = SimAction $ setValueGenerator idx floatIncrementGen
setGen2Act idx = SimAction $ setValueGenerator idx floatDecrementGen

data SimulatorRuntime = SimulatorRuntime
    { simulatorSimHandle :: MVar SimulationHandle
    , simulatorPipe :: SimulatorPipe
    , simulatorModel :: SimulationModel   
    }

runSimulation (SimulatorRuntime handleVar pipe simModel) = do
    h <- startSimulation pipe process simModel
    r1 <- sendRequest pipe (setGen1Act boostersNozzle1T)
    r2 <- sendRequest pipe (setGen2Act boostersNozzle2T)
    let isOk = L.nub [r1, r2] == [Ok]
    if isOk then putMVar handleVar h >> print "Simulation started."
            else print "Simulation failed."
    return isOk

terminateSimulation (SimulatorRuntime handleVar pipe simModel) = do
    h <- takeMVar handleVar
    stopSimulation h
    print "Simulation stopped."

makeSimulatorRuntime = do
    simModel <- compileSimModel networkDef
    pipe <- createPipe :: IO SimulatorPipe
    h <- newEmptyMVar
    return $ SimulatorRuntime h pipe simModel
    
getDevices (SimulatorRuntime handleVar pipe simModel) = do
    ds <- sendRequest pipe GetDevices
    return $ outDevices ds