{-# LANGUAGE RankNTypes #-}
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
interpreter (Pure a) = return a
interpreter (Free proc) = case proc of
    Ask c p next -> do
        print $ "Asked: " ++ show c ++ ", " ++ show p
        interpreter (next trueValue)
    Read c p next -> do
        print $ "Read: " ++ show c ++ ", " ++ show p
        interpreter (next $ toKelvin 100.0)
    ReadCelsius c p next -> do
        print $ "ReadCelsius: " ++ show c ++ ", " ++ show p
        interpreter (next $ toCelsius 55.0)
    ReadKelvin c p next -> do
        print $ "ReadKelvin: " ++ show c ++ ", " ++ show p
        interpreter (next $ toKelvin 3.0)
    Run c cmd next -> do
        print $ "Run: " ++ show c ++ ", " ++ show cmd
        interpreter next
    SendTo rec val next -> do
        print $ "SendTo val: " ++ show val
        rec val
        interpreter next
