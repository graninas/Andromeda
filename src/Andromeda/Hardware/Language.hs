module Andromeda.Hardware.Language where

import Andromeda.Common
import Andromeda.Calculations

newtype Controller = Controller String
  deriving (Show, Read, Eq)

data Property
    = Time
    | Status
  deriving (Show, Read, Eq)

data Parameter tag = Temperature | Pressure
  deriving (Show, Read, Eq)

data Power -- 'Power' units for boosters...

data Command = Command String (Maybe Value)
  deriving (Show, Read, Eq)


toPower :: Int -> Measurment Power
toPower v = Measurment (intValue v)

temperature :: Parameter Kelvin
temperature = Temperature

temperatureKelvin = temperature

temperatureCelsius :: Parameter Celsius
temperatureCelsius = Temperature

pressure :: Parameter Pascal
pressure = Pressure

status = Status

