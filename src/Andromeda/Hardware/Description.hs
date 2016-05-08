module Andromeda.Hardware.Description where

import qualified Data.ByteString.Char8 as BS

type HardwareName = BS.ByteString
type Description = BS.ByteString
type PhysicalAddress = BS.ByteString

data ComponentClass
    = Sensors
    | TerminalUnits
  deriving (Show, Read, Eq)

type Guid = BS.ByteString

-- | Class, manufacturer, description, serial number.
-- Just info.
data ComponentDef = ComponentDef
    { componentClass :: ComponentClass
    , componentGuid  :: Guid 
    , componentManufacturer :: BS.ByteString
    , componentDescritpion  :: HardwareName }
  deriving (Show, Read, Eq)


component = ComponentDef

sensors = Sensors
terminalUnits = TerminalUnits
