module Andromeda.Hardware.Parameter where

import Andromeda.Calculations
import Andromeda.Common

data Parameter tag = Temperature | Pressure
  deriving (Show, Read, Eq)




data Power -- 'Power' units for boosters...



toPower :: Int -> Measurment Power
toPower v = Measurment (intValue v)

temperature :: Parameter Kelvin
temperature = Temperature
pressure :: Parameter Pascal
pressure = Pressure

temperatureKelvin :: Parameter Kelvin
temperatureKelvin = temperature
temperatureCelsius :: Parameter Celsius
temperatureCelsius = Temperature
