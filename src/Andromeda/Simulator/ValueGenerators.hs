{-# LANGUAGE ExistentialQuantification #-}
module Andromeda.Simulator.ValueGenerators where

import Andromeda.Calculations
import Andromeda.Common

data NoiseGenerator = forall a. NoiseGenerator (a -> a)


floatIncrementGen = NoiseGenerator (\(Measurement (FloatValue v)) -> Measurement . FloatValue $ (v+1.0))