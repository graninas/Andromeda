{-# LANGUAGE Arrows #-}
module FreeIOArrowsTest where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Trans.Free
import qualified Control.Monad.Free as F

import Andromeda
import TestCommon
import Lib

type ControlProgramFT a = FreeT Control IO a

evalScriptFT :: Script a -> ControlProgramFT a
evalScriptFT scr = liftF (EvalScript scr id)

type ControlFreeIO = FreeT Control IO
type FlowIOArr b c = ArrEff ControlFreeIO b c
-- This file contains hacks and shortpaths to demonstrate the approach to be designed.

storeValueA :: FlowIOArr DbValue ()
storeValueA = mArr (evalScriptFT . infrastructureScript . storeValue)

-- TODO: implement arrows and scripts poperly.
valueA :: FlowIOArr ValueSource (Measurement Kelvin)
valueA = undefined

-- Just a sample of whatever meaningless computation
integralA :: Float -> FlowIOArr Float Float
integralA v = arr (\v1 -> v + v1)

calculateSomething :: FlowIOArr (Measurement Kelvin) (String, Float)
calculateSomething = proc k -> do
    v <- integralA 0.01 -< 2.0 * (fromKelvin k) -- Some weird computation
    returnA -< ("something", v / 2.0)

seconds n = n * 1000000

monitor :: FlowIOArr () ()
monitor = proc _ -> do
    t1 <- periodicA (seconds 1) valueA -< boostersNozzle1T
    v1 <- calculateSomething -< t1
    storeValueA -< (boostersNozzle1T, t1, v1)
    returnA -< ()

interpretFT :: ControlProgramFT a -> IO a
interpretFT prog = do
    x <- runFreeT prog
    interpretFT' x

interpretFT' (Pure a) = return a
interpretFT' (Free (EvalScript (ControllerScript cs) next)) = do
    v <- interpretControllerScript cs
    interpretFT (next v)
    
runFreeIOArr interpret ar v = do
    let p = runArrEff1 ar v
    --x <- runFreeT p
    (c, next) <- interpret p -- TODO: what to do with next?
    return c

    
test :: IO ()
test = do
    print "LogicControl.Test:"

    runFreeIOArr interpretFT monitor ()



    print "Done."
