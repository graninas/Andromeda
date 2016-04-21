{-# LANGUAGE DeriveDataTypeable #-}

module Andromeda.Hardware.Parameter where

import Andromeda.Calculations
import Andromeda.Common

import Data.Typeable

data Typeable tag => Parameter tag = Temperature | Pressure
  deriving (Show, Read, Eq, Typeable)




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
