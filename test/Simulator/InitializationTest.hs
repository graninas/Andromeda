module Simulator.InitializationTest where

import Lib
import Andromeda
import TestCommon

import Control.Concurrent

data In = SetNoiseGenerator ComponentInstanceIndex NoiseGenerator
data Out = Out String
  deriving (Show, Eq)
  
type SimulatorPipe = Pipe In Out

process req simModel = return (Out "initialized", simModel)

test = do
    let simModel = compileSimModel networkDef
    
    pipe <- createPipe :: IO SimulatorPipe
    thrId <- forkIO $ simulation pipe process simModel

    resp <- sendRequest pipe (SetNoiseGenerator boostersNozzle1T floatIncrementGen)
    if (resp == Out "initialized") then print "passed." else print "failed."
    
    killThread thrId
    print "Simulation done."