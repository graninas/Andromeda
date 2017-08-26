module Andromeda.Types.Physics.Measurement where

import Andromeda.Types.Common.Value

newtype Measurement a = Measurement Value
  deriving (Show, Read, Eq, Ord)

instance ToValue (Measurement a) where
  toValue (Measurement v) = v

toMeasurementV :: Value -> Measurement a
toMeasurementV = Measurement
