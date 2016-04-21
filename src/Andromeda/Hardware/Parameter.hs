{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Andromeda.Hardware.Parameter where

import Andromeda.Calculations
import Andromeda.Common


import Data.Typeable

-- First attempt (used in many modules)
data Parameter tag = Temperature | Pressure
  deriving (Show, Read, Eq)

data Power -- 'Power' units for boosters...

toPower :: Int -> Measurement Power
toPower v = Measurement (intValue v)

temperature :: Parameter Kelvin
temperature = Temperature
pressure :: Parameter Pascal
pressure = Pressure

temperatureKelvin :: Parameter Kelvin
temperatureKelvin = temperature
temperatureCelsius :: Parameter Celsius
temperatureCelsius = Temperature




-- Second attempt (used in HDL)
data Par = Par TypeRep Value
  deriving (Show, Eq)

temperaturePar = Par (typeOf zeroKelvin) (toValue zeroKelvin)
pressurePar    = Par (typeOf zeroPascal) (toValue zeroPascal)
