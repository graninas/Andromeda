module ParameterTagTest where

import Andromeda
import TestCommon

import Prelude hiding (read)
import Control.Monad.Free

-- TODO: add new tests
sendTemperature controller = do
    t <- readTemperature controller nozzle1T
    p <- untag $ readPressure controller nozzle1P
    return (t, p)

test = interpretControllerScript (sendTemperature boostersController)
