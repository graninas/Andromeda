module Andromeda.Hardware.Language where

import Andromeda.System.Language

data Property
    = InternalTime
    | Property
    | Status
    
data Reading = Reading Timestamp Type Value 

data Data = Readings [Reading]
          | Info String
          

data Parameter = Parameter -- lens to access Data!

data Value = BoolValue Bool
           | StringValue String

data Command = Command

data HardwareHint = Hardware Address

boolValue :: Bool -> Value
boolValue = BoolValue

stringValue :: String -> Value
stringValue = StringValue

readings = Readings

info = Info
