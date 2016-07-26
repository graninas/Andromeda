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

start   = Command "start"
stop    = Command "stop"
power _ = Command "power"

logReceiver :: Receiver
logReceiver = \v -> print v

alarmReceiver :: Receiver
alarmReceiver = \v -> print ("WARNING!", v)

readTemperature :: Controller -> ComponentIndex -> ControllerScript (Measurement Kelvin)
readTemperature controller idx = read controller idx temperature

readPressure :: Controller -> ComponentIndex -> ControllerScript (Measurement Pascal)
readPressure controller idx = read controller idx pressure
    
aaaManufacturer = "AAA Inc."
guid1 = "3539390d-f189-4434-bd9e-d39e494a869a"
guid2 = "c80a1ba3-1e7a-4af9-a10b-2313cc10f9e4"
guid3 = "02ecabc3-1a02-481f-8e9a-7b0cc62e38f5"

aaa_p_02 = component sensors guid1 aaaManufacturer "Pressure sensor AAA-P-02"
aaa_t_25 = component sensors guid2 aaaManufacturer "Temperature sensor AAA-T-25"
aaa_controller_01 = component terminalUnits guid3 aaaManufacturer "Controller AAA-C-01"

nozzleTemeratureCompIdx, nozzlePressureCompIdx :: ComponentIndex
nozzle1TCompIdx, nozzle2TCompIdx, nozzle1PCompIdx, nozzle2PCompIdx :: ComponentIndex
rotaryEngineControllerCompIdx, boostersControllerCompIdx :: ComponentIndex
nozzleTemeratureCompIdx = "nozzle-t"
nozzlePressureCompIdx = "nozzle-p"
nozzle1TCompIdx = "nozzle1-t"
nozzle2TCompIdx = "nozzle2-t"
nozzle1PCompIdx = "nozzle1-p"
nozzle2PCompIdx = "nozzle2-P"
rotaryEngineControllerCompIdx = "rotary-engine-controller"
boostersControllerCompIdx = "boosters-controller"

boostersAddr :: PhysicalAddress
boostersAddr = "01:02" -- (row, column)
boostersTUAddr :: PhysicalAddress
boostersTUAddr = "03:02" -- (row, column)

boostersNozzle1T   = (boostersAddr, nozzle1TCompIdx)
boostersNozzle1P   = (boostersAddr, nozzle1PCompIdx)
boostersNozzle2T   = (boostersAddr, nozzle2TCompIdx)
boostersNozzle2P   = (boostersAddr, nozzle2PCompIdx)
boostersController = (boostersAddr, boostersControllerCompIdx)

--rotaryEngine1ObjIdx, rotaryEngine2ObjIdx, boostersObjIdx :: DeviceObjectIndex
--rotaryEngine1ObjIdx = "left-rotary-engine-device"
--rotaryEngine2ObjIdx = "right-rotary-engine-device"
--boostersObjIdx = "boosters-device"

boostersDef :: Hdl ()
boostersDef = do
    sensor aaa_t_25 nozzle1TCompIdx temperaturePar
    sensor aaa_t_25 nozzle2TCompIdx temperaturePar
    sensor aaa_p_02 nozzle1PCompIdx pressurePar
    sensor aaa_p_02 nozzle2PCompIdx pressurePar
    controller aaa_controller_01 boostersControllerCompIdx

rotaryEngineDef :: Hdl ()
rotaryEngineDef = do
    sensor aaa_t_25 nozzleTemeratureCompIdx temperaturePar
    sensor aaa_p_02 nozzlePressureCompIdx   pressurePar
    controller aaa_controller_01 rotaryEngineControllerCompIdx

{-
Sensors            [2t, 2p]
            [1t, 1p]  :   [1t, 1p]
               :      :      :
Devices        R1     B     R2
               :      :      :
01             R1C    BC    R2C
02             |      |      |
03             RTUR1  RTUB  RTUR2 
04              |_____|_____|
05                    |
06                    |
07                    |
08                    |
09                    LC
        00     01     02    03     04
-}

-- terminal units are computers inside network
-- controllers are computers inside device

networkDef :: Hndl ()
networkDef = do
    iBoosters <- remoteDevice boostersAddr boostersDef "boosters"
--    iRotaryEngine1 <- remoteDevice (rotaryEngineDef rotaryEngine1ObjIdx) "left rotary engine"
--    iRotaryEngine2 <- remoteDevice (rotaryEngineDef rotaryEngine2ObjIdx) "right rotary engine"

    iBoostersTU      <- terminalUnit boostersTUAddr "boosters terminal unit"
--    iRotaryEngine1TU <- terminalUnit "02:01" "left rotary engine terminal unit"
--    iRotaryEngine2TU <- terminalUnit "02:03" "right rotary engine terminal unit"
    
    linkedDevice iBoosters iBoostersTU
--    linkedDevice iRotaryEngine1 iRotaryEngine1TU
--    linkedDevice iRotaryEngine2 iRotaryEngine2TU
    
    iLogicControl <- logicControl "08:02" "main logic control"
    
    link iLogicControl iBoostersTU
--    link iLogicControl iRotaryEngine1TU
--    link iLogicControl iRotaryEngine2TU
    

