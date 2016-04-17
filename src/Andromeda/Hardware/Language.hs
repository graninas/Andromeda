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
  
  
temperature :: Parameter Kelvin
temperature = Temperature

pressure :: Parameter Pascal
pressure = Pressure

status = Status

