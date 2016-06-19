module Main where

import qualified SimulatorTest as Sim
import qualified BoostersHeatUpTest as T1
import qualified ParameterTagTest as T2
import qualified HardwareDescriptorsTest as T3
import qualified HardwareNetworkTest as T4
import qualified ParameterTagTest as T5
import qualified LogicControl.Test as T6

main :: IO ()
main = do
    print "Testing..."
      
--    T1.test
--    T2.test
    Sim.test
    T3.test
    T4.test
    T5.test
    T6.test 

