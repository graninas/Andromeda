{-# LANGUAGE OverloadedStrings #-}
module Main where

import Andromeda.LogicControl.Language
import Andromeda.Hardware.Language
import Andromeda.Common.Value

import Prelude hiding (read)
import Control.Monad.Free

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

heatUp :: Controller -> Script ()
heatUp controller = do
        t1 <- read controller temperature
        sendData t1
        run controller start
        run controller (power 1.0)
        run controller stop
        t2 <- read controller temperature
        sendData t2

boostersHeatUp :: Controller -> Script ()
boostersHeatUp controller = do
    st <- ask controller status
    if (st == trueValue)
        then heatUp controller
        else sendReport "Boosters controller is offline."

interpreter :: Script () -> IO ()
interpreter (Pure a) = return a
interpreter (Free proc) = case proc of
    Ask c p next -> do
        print $ "Asked: " ++ show c ++ ", " ++ show p
        interpreter (next trueValue)
    Read c p next -> do
        print $ "Read: " ++ show c ++ ", " ++ show p
        interpreter (next $ floatValue 100.0)
    Run c cmd next -> do
        print $ "Run: " ++ show c ++ ", " ++ show cmd
        interpreter next
    SendTo rec val next -> do
        print $ "SendTo val: " ++ show val
        rec val
        interpreter next

main :: IO ()
main = do
    let controller = Controller "abc"
    interpreter $ boostersHeatUp controller

