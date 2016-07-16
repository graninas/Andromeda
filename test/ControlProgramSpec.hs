module ControlProgramSpec where

import Andromeda
import TestCommon

import Control.Monad.Free
import Test.Hspec

inf = "[INF]"
err = "[ERR]"

controlProgram :: ControlProgram (CommandResult, CommandResult)
controlProgram = do
    logMessage inf "Control program started."
    result1 <- evalScript (controllerScript startBoosters)
    result2 <- evalScript (controllerScript startRotaryEngines)
    checkResult result1
    checkResult result2
    logMessage inf "Control program finished."
    return (result1, result2)
    
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

spec = describe "ControlProgram interpretation test." $
    it "Running start and stop boosters should return OK." $ do
        ((r1, r2), st) <- testInterpretControlProgram False controlProgram
        r1 `shouldBe` (Right "OK.")
        r2 `shouldBe` (Right "OK.")