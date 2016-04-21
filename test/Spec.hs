module Main where

import qualified SimulatorTest as Sim
import qualified BoostersHeatUpTest as T1
import qualified ParameterTagTest as T2
import qualified HardwareDescriptorsTest as T3


main :: IO ()
main = do
    print "Testing..."
      
--    T1.test
--    T2.test
--    Sim.test
    T3.test
    

