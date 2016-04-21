module Andromeda.Calculations.Physics.Units where

import Andromeda.Common.Value
import Andromeda.Calculations.Measurments

data Pascal
data Kelvin
data Celsius


toKelvin :: Float -> Measurment Kelvin
toKelvin = Measurment . floatValue
fromKelvin :: Measurment Kelvin -> Float
fromKelvin (Measurment (FloatValue v)) = v

toCelsius :: Float -> Measurment Celsius
toCelsius = Measurment . floatValue
fromCelsius :: Measurment Celsius -> Float
fromCelsius (Measurment (FloatValue v)) = v

toPascal :: Float -> Measurment Pascal
toPascal = Measurment . floatValue
fromPascal :: Measurment Pascal -> Float
fromPascal (Measurment (FloatValue v)) = v
