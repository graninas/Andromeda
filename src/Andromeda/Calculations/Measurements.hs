module Andromeda.Calculations.Measurements where

import Andromeda.Common.Value

import Data.Typeable
import Data.Data

newtype Measurement a = Measurement Value
  deriving (Show, Read, Eq)
  
instance ToValue (Measurement a) where
  toValue (Measurement v) = v
  

-- TODO: think how to do it
toMeasurement :: TypeRep -> Value -> Measurement a
toMeasurement tr v = undefined

-- or it:
--toMeasurement :: DataType -> Value -> Measurement a
--toMeasurement dt v = undefined
