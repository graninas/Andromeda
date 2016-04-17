module BoostersHeatUpTest where

import Andromeda.LogicControl.Language
import Andromeda.Hardware.Language
import Andromeda.Common.Value

import Common

import Prelude hiding (read)
import Control.Monad.Free

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

test :: IO ()
test = do
    interpreter $ boostersHeatUp boostersController

