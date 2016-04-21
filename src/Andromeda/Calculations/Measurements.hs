module Andromeda.Calculations.Measurements where

import Andromeda.Common.Value

newtype Measurement a = Measurement Value
  deriving (Show, Read, Eq)
