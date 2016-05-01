module Andromeda.Hardware.Description where

import Andromeda.Hardware.Parameter

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS

type HardwareName = BS.ByteString

data DeviceClass
    = Sensors
    | Rtus -- Remote terminal units
    | Ventiles
  deriving (Show, Read, Eq)

-- | Pasport of device: class of device, manufacturer, description, serial number and other.
-- Just info.
data DeviceDescription = DeviceDescription
    { _deviceClass :: DeviceClass
    , _deviceGuid :: BS.ByteString
    , _deviceName :: HardwareName
    , _deviceManufacturer :: BS.ByteString }
  deriving (Show, Read, Eq)


device = DeviceDescription


