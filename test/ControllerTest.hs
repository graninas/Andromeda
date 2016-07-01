module ControllerTest where

import Control.Monad.Free

import Andromeda
import TestCommon

inf = "[INF]"
err = "[ERR]"

controlProgram :: ControlProgram ()
controlProgram = do
    logMessage inf "Control program started."
    result1 <- evalScript (controllerScript startBoosters)
    result2 <- evalScript (controllerScript startRotaryEngines)
    checkResult result1
    checkResult result2
    logMessage inf "Control program finished."
    
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
    let errorMsg = "Start engines failed"
    logMessage err errorMsg
    evalScript (infrastructureScript (alarm errorMsg))
checkResult (Right succeeded) = 
    logMessage inf "Start engines succeeded"

logMsg :: String -> InfrastructureScript ()
logMsg = sendTo logReceiver . StringValue

alarm :: String -> InfrastructureScript ()
alarm = sendTo alarmReceiver . StringValue

test :: TestCPInterpreter ()
test = interpretControlProgram controlProgram