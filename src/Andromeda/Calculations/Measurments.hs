module Andromeda.Calculations.Measurments where

import Andromeda.Common.Value

newtype Measurment a = Measurment Value
  deriving (Show, Read, Eq)
