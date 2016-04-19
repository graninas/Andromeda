{-# LANGUAGE Arrows #-}
module SimulatorTest where

import Andromeda.Simulator.Facade
import Andromeda.Common.Value
import Andromeda.Hardware.Language

import Control.Arrow.Transformer.Automaton
import Control.Arrow
import Control.Lens
import qualified Data.Stream as St

{-
boostersModel :: SimulationModel
boostersModel = do
    t <- parameter temperature
    p <- parameter power
    s <- parameter status
    s1 <- state [s `is` "off", t > 0] (cooldown t)
-}

boostersModel :: SimulationModel State
boostersModel = proc (s, i) -> do
--    t <- arr (parameter stTemperature) -< s
--    p <- parameter power       -< s
    let upd = over stTemperature (increaseTemperature 1.0)

    returnA -< upd s

    


boostersSimulation :: Simulation
boostersSimulation = simulation "Boosters" boostersModel

initialState = State (toKelvin 0.0) (toPower 0)

run = do
    let inputStream = St.fromList [1..]
    let f = runAutomaton boostersModel
    let out = f (initialState, inputStream)
    return $ St.take 3 $ out

test :: IO ()
test = do
    n <- run
    mapM_ (\t -> print "\n" >> print t) n
