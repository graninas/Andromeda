{-# LANGUAGE RankNTypes #-}
module BoostersHeatUpTest where

import Andromeda

import TestCommon

import Prelude hiding (read)
import Control.Monad.Free

-- TODO: create new tests

--heatUp :: Controller -> ControllerScript ()
heatUp controller = do
        t1 <- untag $ readTemperature controller
--        sendData (floatValue t1)
        run controller start
        run controller (power 1.0)
        run controller stop
        t2 <- untag $ readTemperature controller
--        sendData (floatValue t2)
        t3 <- untag $ readTemperatureCelsius controller
--        sendData (floatValue t3)
        t4 <- untag $ readTemperatureKelvin controller
--        sendData (floatValue t4)
        return (t1, t2, t3, t4)

--boostersHeatUp :: Monad m => Controller -> m ()
boostersHeatUp controller = do
    st <- get controller status
    if (st == trueValue)
        then heatUp controller
        else return (0.0, 0.0, 0.0, 0.0) --sendReport "Boosters controller is offline."

test :: IO ()
test = do
    vs <- interpretControllerScript $ boostersHeatUp boostersController
    print vs

