{-# LANGUAGE RankNTypes #-}
module Main where

import qualified BoostersHeatUpTest as T1
import qualified ParameterTagTest as T2
import qualified SimulatorTest as Sim

import Andromeda.LogicControl.Language
import Andromeda.Hardware.Language
import Andromeda.Common.Value

import Common

import Prelude hiding (read)
import Control.Monad.Free



main :: IO ()
main = do
    print "Testing..."
      
--    T1.test
--    T2.test
    Sim.test
    

