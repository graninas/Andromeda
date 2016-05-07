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

saveData :: Value -> Script ()
saveData = sendTo database
sendReport :: String -> Script ()
sendReport s = sendTo reporter (stringValue s)
sendData :: Value -> Script ()
sendData v = saveData v >> sendReport ("sending: " ++ show v)

start   = Command "start" Nothing
stop    = Command "stop" Nothing
power f = Command "power" (Just $ floatValue f)

readTemperature :: Controller -> ScriptT Kelvin Float
readTemperature controller = do
    t <- read controller temperature
    return $ fromKelvin t

readTemperatureCelsius :: Controller -> ScriptT Celsius Float
readTemperatureCelsius controller = do
    t <- readCelsius controller temperatureCelsius
    return $ fromCelsius t

readTemperatureKelvin :: Controller -> ScriptT Kelvin Float
readTemperatureKelvin controller = do
    t <- readKelvin controller temperatureKelvin
    return $ fromKelvin t

readTemperatureKelvin1 :: Controller -> ScriptM Kelvin
readTemperatureKelvin1 controller = do
    readKelvin controller temperatureKelvin
    
    
readPressure :: Controller -> ScriptT Pascal Float
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
--interpreter :: Script () -> IO ()
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

rtuDef :: Hdl ()
rtuDef = rtu aaa_rtu_01 "rtu"

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
    rtuR1 <- terminalUnit "02:01" rtuDef "rtu1-left rotary engine" 
    rtuRB <- terminalUnit "02:02" rtuDef "rtub-boosters"
    rtuR2 <- terminalUnit "02:03" rtuDef "rtu2-right rotary engine"
    iRtu  <- terminalUnit "04:02" rtuDef "intermediate rtu"
    rotE1 <- remoteDevice "00:01" rotaryEngineDef "left rotary engine"
    rotE2 <- remoteDevice "00:03" rotaryEngineDef "right rotary engine"
    boost <- remoteDevice "00:02" boostersDef "boosters"
    lc    <- logicControl "08:02" "main logic control"
    connection [rotE1, rtuR1, iRtu] "conn to left rot e"
    connection [rotE2, rtuR2, iRtu] "conn to right rot e"
    connection [boost, rtuRB, iRtu] "conn to boosters"
    connection [iRtu, lc] "conn to iRtu"
    
    
    

