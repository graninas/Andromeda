{-# LANGUAGE ExistentialQuantification #-}
module Andromeda.Simulator.ValueGenerators where

import Andromeda.Calculations
import Andromeda.Common

data ValueGenerator = NoGenerator
                    | forall a. NoiseGenerator (a -> a)


noGenerator = NoGenerator
floatIncrementGen = NoiseGenerator (\(Measurement (FloatValue v)) -> Measurement . FloatValue $ (v+1.0))