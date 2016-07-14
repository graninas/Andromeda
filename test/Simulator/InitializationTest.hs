module Simulator.InitializationTest where

import Lib
import Andromeda
import TestCommon

import Control.Concurrent

data In = SetValueGenerator ComponentInstanceIndex ValueGenerator
data Out = Out String
  deriving (Show, Eq)
  
type SimulatorPipe = Pipe In Out

process _ = return $ Out "initialized"

test = do
    simModel <- compileSimModel networkDef
    
    pipe <- createPipe :: IO SimulatorPipe
    simHandle <- startSimulation pipe process simModel

    resp <- sendRequest pipe (SetValueGenerator boostersNozzle1T floatIncrementGen)
    if (resp == Out "initialized") then print "passed." else print "failed."
    
    stopSimulation simHandle
    print "Simulation done."