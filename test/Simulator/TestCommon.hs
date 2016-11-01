module Simulator.TestCommon where

import Lib
import Andromeda
import TestCommon

-- Can type families be used here?

data In = Initialize
        | SimAction (SimState ())
        | GetValueSource ComponentInstanceIndex
        | GetHardwareHandle
data Out = Ok
         | OutValueSource ValueSource
         | OutHardwareHandle SimHardwareHandle

instance Eq Out where
    Ok == Ok = True
    OutValueSource v1 == OutValueSource v2 = False
    _ == _ = False
  
instance Show Out where
    show Ok = "OK"
    show (OutValueSource _) = "OutValueSource"
    show (OutHardwareHandle _) = "OutHardwareHandle"
  
type SimulatorPipe = Pipe In Out

ok = Ok

process :: Process In Out
process Initialize = return ok
process (SimAction act) = act >> return ok
process (GetValueSource idx) = getValueSource idx >>= (return . OutValueSource)
process GetHardwareHandle = getHardwareHandle >>= (return . OutHardwareHandle)

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
