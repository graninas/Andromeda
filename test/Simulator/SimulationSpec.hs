{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module Simulator.SimulationSpec where

import Lib
import Andromeda
import TestCommon

import Test.Hspec
import Control.Monad
import Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Lens

data In = Initialize
        | SimStateSimpleAction (SimState ())
        | forall v. SimStateReturningAction (SimState v)
        | Start ComponentInstanceIndex
        | Stop ComponentInstanceIndex
data Out = Out String
         | forall v . OutValue v

instance Eq Out where
    Out s1 == Out s2 = s1 == s2
    OutValue v1 == OutValue v2 = False -- TODO
    _ == _ = False
  
instance Show Out where
    show (Out s1) = "Out " ++ s1
    show (OutValue v1) = "OutValue"
  
type SimulatorPipe = Pipe In Out

ok = Out "OK."

process :: Process In Out
process Initialize = return ok
process (SimStateSimpleAction act) = act >> return ok
process (SimStateReturningAction act) = do
    v <- act
    return $ OutValue v

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
    
okOnSuccessAction act = SimStateSimpleAction act
valueOnSuccessAction act = SimStateReturningAction act

runNetworkAct = okOnSuccessAction $ runNetwork
setGen1Act idx = okOnSuccessAction $ setValueGenerator idx floatIncrementGen
getValueSourceAct idx = valueOnSuccessAction $ getValueSource idx

{-
newtype SimNetworkBridge a = SimNetworkBridge (StateT InterpreterSt IO a)
    deriving (Monad, MonadState, MonadIO)

instance ControlProgramInterpreter SimulatorBridge where
    onEvalScript (ControllerScript scr)     = interpretControllerScript scr
    onEvalScript (InfrastructureScript scr) = interpretInfrastructureScript scr

instance ControllerScriptInterpreter SimulatorBridge where
    onGet c p = do
        debugPrint_ ("Get", c, p)
        error "onGet not implemented."
    onSet c p v = do
        debugPrint_ ("Set", c, p, v)
        error "onSet not implemented."
    onRead contr compIdx param = do
        debugPrint_ ("Read", contr, compIdx, param)
        error "onRead not implemented."
    onRun c cmd = do
        debugPrint_ ("Run", c, cmd)
        error "onRun not implemented."
    
instance InfrastructureScriptInterpreter TestCPInterpreter where
    onSendTo r v = do
        debugPrint_ ("SendTo", v)
        error "onSendTo not implemented."
    onGetCurrentTime = do
        debugPrint_ "GetCurrentTime"
        error "onGetCurrentTime not implemented."

readSensor' :: ComponentInstanceIndex -> ControlProgram [Measurement Kelvin]
readSensor' (pa, idx) = do
    result1 <- evalScript (controllerScript startBoosters)
    result2 <- evalScript (controllerScript startRotaryEngines)
    checkResult result1
    checkResult result2
    logMessage inf "Control program finished."
    return (result1, result2)

readSensorTimes :: Int -> ComponentInstanceIndex -> ControlProgram [Measurement Kelvin]
readSensorTimes n = sequence . replicate n . readSensor'
    
logMessage :: String -> String -> ControlProgram ()
logMessage severity str = do
    time <- evalScript (infrastructureScript getCurrentTime)
    let msg = show (time, severity, str)
    evalScript (infrastructureScript (logMsg msg))
    
startBoosters :: ControllerScript CommandResult
startBoosters = run (Controller "boosters") (Command "start")

startRotaryEngines :: ControllerScript CommandResult
startRotaryEngines = run (Controller "rotary engines") (Command "start")

checkResult :: CommandResult -> ControlProgram ()
checkResult (Left failed) = do
    let errorMsg = "Command failed"
    logMessage err errorMsg
    evalScript (infrastructureScript (alarm errorMsg))
checkResult (Right succeeded) = 
    logMessage inf "Command succeeded"

logMsg :: String -> InfrastructureScript ()
logMsg = sendTo logReceiver . StringValue

alarm :: String -> InfrastructureScript ()
alarm = sendTo alarmReceiver . StringValue
    -}
    
    
    
    
    
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
        (OutValue vs) <- sendRequest pipe (getValueSourceAct boostersNozzle1T)
        
        stopSimulation simHandle
        r1 `shouldBe` ok
        r2 `shouldBe` ok
    -- TODO: setting value gen to non-existent sensor
    