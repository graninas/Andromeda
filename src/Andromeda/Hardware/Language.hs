module Andromeda.Hardware.Language where

import Andromeda.Common.Value

data Controller = Controller String
  deriving (Show, Read, Eq)

data Property
    = Time
    | Status
  deriving (Show, Read, Eq)

data Parameter = Temperature | Pressure
  deriving (Show, Read, Eq)

data Command = Command String (Maybe Value)
  deriving (Show, Read, Eq)
  
  
  
temperature = Temperature
pressure = Pressure
status = Status

