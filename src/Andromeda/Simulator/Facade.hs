{-# LANGUAGE TemplateHaskell #-}
module Andromeda.Simulator.Facade where

import Andromeda.Hardware
import Andromeda.Common
import Andromeda.Calculations

import Control.Concurrent
import Control.Lens
import Control.Lens.TH
import Control.Arrow.Transformer.Automaton

-- First try to desing simulator
{-
data State = State {
    _stTemperature :: Measurement Kelvin,
    _stPower       :: Measurement Power
    }
  deriving (Show)

type Iteration = Int
type Auto = Automaton (->)
type SimulationModel a = Auto (State, Iteration) a

data Simulation = Simulation String (SimulationModel State)

makeLenses ''State

increaseTemperature :: Float -> Measurement Kelvin -> Measurement Kelvin
increaseTemperature i (Measurement (FloatValue f)) =
    Measurement (FloatValue (f + i))

simulation = Simulation

-}