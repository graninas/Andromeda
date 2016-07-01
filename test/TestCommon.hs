{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module TestCommon where

import Andromeda

import Prelude hiding (read)
import Control.Monad.Free

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

type TestCPInterpreter = IO

instance ControlProgramInterpreter TestCPInterpreter where
    onEvalScript (ControllerScript scr)     = interpretControllerScript scr
    onEvalScript (InfrastructureScript scr) = interpretInfrastructureScript scr

instance ControllerScriptInterpreter TestCPInterpreter where
    onGet c p     = print ("Get", c, p)      >> return (StringValue "ggg")
    onSet c p v   = print ("Set", c, p, v)
    onRead c ci p = print ("Read", c, ci, p) >> return (Measurement . FloatValue $ 33.3)
    onRun c cmd   = print ("Run", c, cmd)    >> return (Right "OK.")
    
instance InfrastructureScriptInterpreter TestCPInterpreter where
    onSendTo r v     = print ("SendTo", v)
    onGetCurrentTime = print "GetCurrentTime" >> return 10
        
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

networkDef :: Hndl ()
networkDef = do
    controllerR1 <- terminalUnit "02:01" controllerDef "controller1-left rotary engine" 
    controllerRB <- terminalUnit "02:02" controllerDef "controllerb-boosters"
    controllerR2 <- terminalUnit "02:03" controllerDef "controller2-right rotary engine"
    iController  <- terminalUnit "04:02" controllerDef "intermediate controller"
    rotE1 <- remoteDevice "00:01" rotaryEngineDef "left rotary engine"
    rotE2 <- remoteDevice "00:03" rotaryEngineDef "right rotary engine"
    boost <- remoteDevice "00:02" boostersDef "boosters"
    lc    <- logicControl "08:02" "main logic control"
    connection [rotE1, controllerR1, iController] "conn to left rot e"
    connection [rotE2, controllerR2, iController] "conn to right rot e"
    connection [boost, controllerRB, iController] "conn to boosters"
    connection [iController, lc] "conn to iController"
    
-- boostersNozzle1T, boostersNozzle2T :: ValueSource Temperature
-- boostersNozzle1P, boostersNozzle2P :: ValueSource Pressure
boostersNozzle1T, boostersNozzle2T :: ValueSource
boostersNozzle1P, boostersNozzle2P :: ValueSource
boostersNozzle1T = ("02:02", nozzle1T)
boostersNozzle1P = ("02:02", nozzle1P)
boostersNozzle2T = ("02:02", nozzle2T)
boostersNozzle2P = ("02:02", nozzle2P)


