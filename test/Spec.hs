module Main where

import qualified BoostersHeatUpTest as T1
import qualified ParameterTagTest as T2
import qualified HardwareDescriptorsTest as T3
import qualified HardwareNetworkTest as T4
import qualified ParameterTagTest as T5
import qualified ArrowsTest as T6
import qualified FreeIOArrowsTest as T7
import qualified ReactiveArrowsTest as T8
import qualified ControllerTest as T9
import qualified ParsingTest as T10
import qualified TranslationTest as T11

import qualified Simulator.AutomationTest as Sim1
import qualified Simulator.InitializationTest as Sim2

main :: IO ()
main = do
    print "Testing..."
    {-
    T1.test
    T2.test
    T5.test
    T6.test
    Sim1.test
    Sim2.test
        -}
    --T3.test
    --T4.test
    
    --T7.test
    --T8.test
    --T9.test
    --T10.test
    T11.test
    