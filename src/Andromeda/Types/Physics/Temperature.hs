module Andromeda.Types.Physics.Temperature where

import Andromeda.Types.Common.Value
import Andromeda.Types.Physics.Measurement

data Pascal
data Kelvin
data Celsius

-- TODO: replace Ad-hoc by instances of classes.
addKelvin :: Measurement Kelvin -> Measurement Kelvin -> Measurement Kelvin
addKelvin (Measurement (FloatValue v1)) (Measurement (FloatValue v2)) = toKelvin (v1 + v2)


toKelvin :: Float -> Measurement Kelvin
toKelvin = Measurement . floatValue
toKelvinV :: Value -> Measurement Kelvin
toKelvinV v@(FloatValue _) = Measurement v
toKelvinV _ = error "bad cast"

fromKelvin :: Measurement Kelvin -> Float
fromKelvin (Measurement (FloatValue v)) = v
zeroKelvin = toKelvin 0.0

toCelsius :: Float -> Measurement Celsius
toCelsius = Measurement . floatValue
toCelsiusV :: Value -> Measurement Celsius
toCelsiusV v@(FloatValue _) = Measurement v
fromCelsius :: Measurement Celsius -> Float
fromCelsius (Measurement (FloatValue v)) = v
zeroCelsius = toCelsius 0.0

toPascal :: Float -> Measurement Pascal
toPascal = Measurement . floatValue
toPascalV :: Value -> Measurement Pascal
toPascalV v@(FloatValue _) = Measurement v
toPascalV _ = error "bad cast"
fromPascal :: Measurement Pascal -> Float
fromPascal (Measurement (FloatValue v)) = v
zeroPascal = toPascal 0.0
