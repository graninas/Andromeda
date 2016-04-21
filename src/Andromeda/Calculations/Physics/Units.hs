module Andromeda.Calculations.Physics.Units where

import Andromeda.Common.Value
import Andromeda.Calculations.Measurements

data Pascal
data Kelvin
data Celsius


toKelvin :: Float -> Measurement Kelvin
toKelvin = Measurement . floatValue
fromKelvin :: Measurement Kelvin -> Float
fromKelvin (Measurement (FloatValue v)) = v

toCelsius :: Float -> Measurement Celsius
toCelsius = Measurement . floatValue
fromCelsius :: Measurement Celsius -> Float
fromCelsius (Measurement (FloatValue v)) = v

toPascal :: Float -> Measurement Pascal
toPascal = Measurement . floatValue
fromPascal :: Measurement Pascal -> Float
fromPascal (Measurement (FloatValue v)) = v
