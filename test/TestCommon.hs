{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module TestCommon where

import Andromeda.Common
import Andromeda.LogicControl
import Andromeda.Calculations
import Andromeda.Assets
import Andromeda.Hardware.Types
import Andromeda.Hardware.Description
import Andromeda.Hardware.Parameter
import Andromeda.Hardware.HDL
import Andromeda.Hardware.HNDL

import Prelude hiding (read)
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad.Free
import Control.Monad.Trans.State as S
import Control.Monad.State.Class (MonadState(..))
import Control.Lens

newtype InterpreterSt = InterpreterSt
    { _debugPrintEnabled :: Bool }

type TestCPInterpreter = StateT InterpreterSt IO

makeLenses ''InterpreterSt

debugPrint_ :: (Show v, MonadState InterpreterSt m, MonadIO m) => v -> m ()
debugPrint_ v = do
    dp <- use debugPrintEnabled
    if dp then liftIO $ print v
          else return ()
          
instance ControlProgramInterpreter TestCPInterpreter where
    onEvalScript (ControllerScript scr)     = interpretControllerScript scr
    onEvalScript (InfrastructureScript scr) = interpretInfrastructureScript scr

instance ControllerScriptInterpreter TestCPInterpreter where
    onGet c p     = debugPrint_ ("Get", c, p)      >> return (StringValue "ggg")
    onSet c p v   = debugPrint_ ("Set", c, p, v)
    onRead c ci p = debugPrint_ ("Read", c, ci, p) >> return (Measurement . FloatValue $ 33.3)
    onRun c cmd   = debugPrint_ ("Run", c, cmd)    >> return (Right "OK.")
    
instance InfrastructureScriptInterpreter TestCPInterpreter where
    onSendTo r v     = debugPrint_ ("SendTo", v)
    onGetCurrentTime = debugPrint_ "GetCurrentTime" >> return 10

testInterpretControllerScript     debugPrint script = runStateT (interpretControllerScript script)     (InterpreterSt debugPrint)
testInterpretInfrastructureScript debugPrint script = runStateT (interpretInfrastructureScript script) (InterpreterSt debugPrint)
testInterpretControlProgram       debugPrint script = runStateT (interpretControlProgram script)       (InterpreterSt debugPrint)
    
boostersController = Controller "Boosters"

start   = Command "start"
stop    = Command "stop"
power f = Command "power" -- (Just $ floatValue f)

logReceiver :: Receiver
logReceiver = \v -> print v

alarmReceiver :: Receiver
alarmReceiver = \v -> print ("WARNING!", v)

readTemperature :: Controller -> ComponentIndex -> ControllerScript (Measurement Kelvin)
readTemperature controller idx = read controller idx temperature

readPressure :: Controller -> ComponentIndex -> ControllerScript (Measurement Pascal)
readPressure controller idx = read controller idx pressure
    
nozzleTemerature, nozzlePressure, nozzle1T, nozzle2T, nozzle1P, nozzle2P :: ComponentIndex
nozzleTemerature = "nozzle-t"
nozzlePressure = "nozzle-p"
nozzle1T = "nozzle1-t"
nozzle2T = "nozzle2-t"
nozzle1P = "nozzle1-p"
nozzle2P = "nozzle2-P"

controllerDef :: Hdl ()
controllerDef = controller aaa_controller_01 "controller"

boostersDef :: Hdl ()
boostersDef = do
    sensor aaa_t_25 nozzle1T temperaturePar
    sensor aaa_t_25 nozzle2T temperaturePar
    sensor aaa_p_02 nozzle1P pressurePar
    sensor aaa_p_02 nozzle2P pressurePar

rotaryEngineDef :: Hdl ()
rotaryEngineDef = do
    sensor aaa_t_25 nozzleTemerature temperaturePar
    sensor aaa_p_02 nozzlePressure   pressurePar

{-  00     01     02    03     04

00         R1     B     R2
01         |      |     |
02         RTUR1  RTUB  RTUR2 
03           |____|_____|
04                IRTU
05                |
06                |
07                |
08                LC
-}

boostersAddr = "00:02" -- (row, column)

networkDef :: Hndl ()
networkDef = do
    controllerR1 <- terminalUnit "02:01" controllerDef "controller1-left rotary engine" 
    controllerRB <- terminalUnit "02:02" controllerDef "controllerb-boosters"
    controllerR2 <- terminalUnit "02:03" controllerDef "controller2-right rotary engine"
    iController  <- terminalUnit "04:02" controllerDef "intermediate controller"
    rotE1 <- remoteDevice "00:01" rotaryEngineDef "left rotary engine"
    rotE2 <- remoteDevice "00:03" rotaryEngineDef "right rotary engine"
    boost <- remoteDevice boostersAddr boostersDef "boosters"
    lc    <- logicControl "08:02" "main logic control"
    connection [rotE1, controllerR1, iController] "conn to left rot e"
    connection [rotE2, controllerR2, iController] "conn to right rot e"
    connection [boost, controllerRB, iController] "conn to boosters"
    connection [iController, lc] "conn to iController"
    
boostersNozzle1T, boostersNozzle2T :: ComponentInstanceIndex
boostersNozzle1P, boostersNozzle2P :: ComponentInstanceIndex
boostersNozzle1T = (boostersAddr, nozzle1T)
boostersNozzle1P = (boostersAddr, nozzle1P)
boostersNozzle2T = (boostersAddr, nozzle2T)
boostersNozzle2P = (boostersAddr, nozzle2P)


