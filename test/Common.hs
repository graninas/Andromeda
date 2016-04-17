{-# LANGUAGE RankNTypes #-}
module Common where

import Andromeda.LogicControl.Language
import Andromeda.Hardware.Language
import Andromeda.Common.Value

import Prelude hiding (read)
import Control.Monad.Free

boostersController = Controller "Boosters"

database :: Value -> IO ()
database v = print $ "Sended to DB: " ++ show v

reporter :: Value -> IO ()
reporter v = print $ "Reported: " ++ show v

--saveData :: Value -> Script None ()
saveData = sendTo database
--sendReport :: String -> Script None ()
sendReport s = sendTo reporter (stringValue s)
--sendData :: Value -> Script None ()
sendData v = saveData v >> sendReport ("sending: " ++ show v)

start   = Command "start" Nothing
stop    = Command "stop" Nothing
power f = Command "power" (Just $ floatValue f)

--interpreter :: forall tag. Script tag () -> IO ()
interpreter (Pure a) = return a
interpreter (Free proc) = case proc of
    Ask c p next -> do
        print $ "Asked: " ++ show c ++ ", " ++ show p
        interpreter (next trueValue)
    Read c p next -> do
        print $ "Read: " ++ show c ++ ", " ++ show p
        interpreter (next $ toKelvin 100.0)
    Run c cmd next -> do
        print $ "Run: " ++ show c ++ ", " ++ show cmd
        interpreter next
    SendTo rec val next -> do
        print $ "SendTo val: " ++ show val
        rec val
        interpreter next
