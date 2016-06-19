{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module TestCommon where

import Andromeda

import Prelude hiding (read)
import Control.Monad.Free

boostersController = Controller "Boosters"

database :: Value -> IO ()
database v = print $ "Sended to DB: " ++ show v

reporter :: Value -> IO ()
reporter v = print $ "Reported: " ++ show v

saveData :: Value -> ControllerScript ()
saveData = sendTo database
sendReport :: String -> ControllerScript ()
sendReport s = sendTo reporter (stringValue s)
sendData :: Value -> ControllerScript ()
sendData v = saveData v >> sendReport ("sending: " ++ show v)

start   = Command "start" Nothing
stop    = Command "stop" Nothing
power f = Command "power" (Just $ floatValue f)

readTemperature :: Controller -> ControllerScriptT Kelvin Float
readTemperature controller = do
    t <- read controller temperature
    return $ fromKelvin t

readTemperatureCelsius :: Controller -> ControllerScriptT Celsius Float
readTemperatureCelsius controller = do
    t <- readCelsius controller temperatureCelsius
    return $ fromCelsius t

readTemperatureKelvin :: Controller -> ControllerScriptT Kelvin Float
readTemperatureKelvin controller = do
    t <- readKelvin controller temperatureKelvin
    return $ fromKelvin t

readTemperatureKelvin1 :: Controller -> ControllerScriptM Kelvin
readTemperatureKelvin1 controller = do
    readKelvin controller temperatureKelvin
    
    
readPressure :: Controller -> ControllerScriptT Pascal Float
readPressure controller = do
    t <- read controller pressure
    return $ fromPascal t

{-
--Couldn't match type ‘Kelvin’ with ‘Celsius’
--impossible :: Monad m => Controller -> m Float
impossible controller = do
    t <- read controller temperature
    return $ fromCelsius t
-}


-- Mocking interpreter for tests.
--interpreter :: ControllerScript () -> IO ()
scriptInterpreter (Pure a) = return a
scriptInterpreter (Free proc) = case proc of
    Ask c p next -> do
        print $ "Asked: " ++ show c ++ ", " ++ show p
        scriptInterpreter (next trueValue)
    Read c p next -> do
        print $ "Read: " ++ show c ++ ", " ++ show p
        scriptInterpreter (next $ toKelvin 100.0)
    ReadCelsius c p next -> do
        print $ "ReadCelsius: " ++ show c ++ ", " ++ show p
        scriptInterpreter (next $ toCelsius 55.0)
    ReadKelvin c p next -> do
        print $ "ReadKelvin: " ++ show c ++ ", " ++ show p
        scriptInterpreter (next $ toKelvin 3.0)
    Run c cmd next -> do
        print $ "Run: " ++ show c ++ ", " ++ show cmd
        scriptInterpreter next
    SendTo rec val next -> do
        print $ "SendTo val: " ++ show val
        rec val
        scriptInterpreter next

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


