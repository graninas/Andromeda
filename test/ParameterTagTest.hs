module ParameterTagTest where

import Andromeda
import TestCommon

import Prelude hiding (read)
import Control.Monad.Free

sendTemperature controller = do
    t <- readTemperature controller
    sendData (floatValue t)
    p <- untag $ readPressure controller
    sendData (floatValue p)

test = interpreter (sendTemperature boostersController)
