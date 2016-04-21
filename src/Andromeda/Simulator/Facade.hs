{-# LANGUAGE TemplateHaskell #-}
module Andromeda.Simulator.Facade where

import Andromeda.Hardware
import Andromeda.Common
import Andromeda.Calculations

import Control.Concurrent
import Control.Lens
import Control.Lens.TH
import Control.Arrow.Transformer.Automaton

data State = State {
    _stTemperature :: Measurment Kelvin,
    _stPower :: Measurment Power
    }
  deriving (Show)

type Iteration = Int
type Auto = Automaton (->)
type SimulationModel a = Auto (State, Iteration) a

data Simulation = Simulation String (SimulationModel State)

makeLenses ''State

increaseTemperature :: Float -> Measurment Kelvin -> Measurment Kelvin
increaseTemperature i (Measurment (FloatValue f)) =
    Measurment (FloatValue (f + i))

simulation = Simulation
