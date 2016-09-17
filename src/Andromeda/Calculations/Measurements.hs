module Andromeda.Calculations.Measurements where

import Andromeda.Common.Value

import Data.Typeable
import Data.Data

newtype Measurement a = Measurement Value
  deriving (Show, Read, Eq, Ord)
  
instance ToValue (Measurement a) where
  toValue (Measurement v) = v

toMeasurementV :: Value -> Measurement a
toMeasurementV = Measurement
