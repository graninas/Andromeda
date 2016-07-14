module Simulator.SimulationTest where

import Lib
import Andromeda
import TestCommon

data In = SetValueGenerator ComponentInstanceIndex ValueGenerator
        | Start ComponentInstanceIndex
        | Stop ComponentInstanceIndex
data Out = NoOut
         | Out String
  deriving (Show, Eq)
  
type SimulatorPipe = Pipe In Out


process :: Process In Out
process (SetValueGenerator idx g) = do
    setValueGenerator idx g
    return NoOut
--process req simModel = return (Out "initialized", simModel)

test = do
    simModel <- compileSimModel networkDef
    
    pipe <- createPipe :: IO SimulatorPipe
    simHandle <- startSimulation pipe process simModel

    resp <- sendRequest pipe (SetValueGenerator boostersNozzle1T floatIncrementGen)
    if (resp == NoOut) then print "passed." else print "failed."
    
    stopSimulation simHandle
    print "Simulation done."