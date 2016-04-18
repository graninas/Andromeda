module Andromeda.Hardware.Language where

import Andromeda.Common.Value

data Controller = Controller String
  deriving (Show, Read, Eq)

data Property
    = Time
    | Status
  deriving (Show, Read, Eq)

data Parameter tag = Temperature | Pressure
  deriving (Show, Read, Eq)

data Measurment a = Measurment Value

data Pascal
data Kelvin
data Celsius

data Command = Command String (Maybe Value)
  deriving (Show, Read, Eq)

toKelvin :: Float -> Measurment Kelvin
toKelvin v = Measurment (floatValue v)

toCelsius :: Float -> Measurment Celsius
toCelsius v = Measurment (floatValue v)

fromKelvin :: Measurment Kelvin -> Float
fromKelvin (Measurment (FloatValue v)) = v
fromCelsius :: Measurment Celsius -> Float
fromCelsius (Measurment (FloatValue v)) = v
fromPascal :: Measurment Pascal -> Float
fromPascal (Measurment (FloatValue v)) = v

temperature :: Parameter Kelvin
temperature = Temperature

temperatureKelvin = temperature

temperatureCelsius :: Parameter Celsius
temperatureCelsius = Temperature

pressure :: Parameter Pascal
pressure = Pressure

status = Status

