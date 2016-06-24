{-# LANGUAGE Arrows #-}
module ArrowsTest where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Free
import qualified Control.Monad.Trans.Free as FT

import Andromeda
import TestCommon
import Lib

-- This file contains hacks and shortpaths to demonstrate the approach to be designed.

storeValueA :: FlowArr DbValue ()
storeValueA = mArr (evalScript . infrastructureScript . storeValue)

-- TODO: implement arrows and scripts poperly.
valueA :: FlowArr ValueSource (Measurement Kelvin)
valueA = undefined

-- Just a sample of whatever meaningless computation
integralA :: Float -> FlowArr Float Float
integralA v = arr (\v1 -> v + v1)

calculateSomething :: FlowArr (Measurement Kelvin) (String, Float)
calculateSomething = proc k -> do
    v <- integralA 0.01 -< 2.0 * (fromKelvin k) -- Some weird computation
    returnA -< ("something", v / 2.0)

periodicA'' = undefined
seconds n = n * 1000000

monitor :: FlowArr () ()
monitor = proc _ -> do
    t1 <- periodicA'' (seconds 1) valueA -< boostersNozzle1T
    v1 <- calculateSomething -< t1
    storeValueA -< (boostersNozzle1T, t1, v1)
    returnA -< ()

interpret' :: ControlProgram a -> IO a
interpret' (Pure a) = return a
interpret' (Free (EvalScript (ControllerScript cs) next)) = do
    v <- interpretControllerScript cs
    interpret' (next v)
    
test :: IO ()
test = do
    print "LogicControl.Test:"

    --runFreeArr interpret' monitor ()



    print "Done."
