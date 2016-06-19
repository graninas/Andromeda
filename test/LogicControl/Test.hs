{-# LANGUAGE Arrows #-}
module LogicControl.Test where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Free

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

seconds n = n * 1000000
    
periodicA :: Int -> FlowArr b c -> FlowArr b c
periodicA = undefined
    
monitor :: FlowArr () ()
monitor = proc _ -> do
    t1 <- periodicA (seconds 1) valueA -< boostersNozzle1T
    v1 <- calculateSomething -< t1
    storeValueA -< (boostersNozzle1T, t1, v1)
    returnA -< ()

test = do
    print "LogicControl.Test:"

    



    print "Done."
