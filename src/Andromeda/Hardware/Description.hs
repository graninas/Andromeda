module Andromeda.Hardware.Description where

import Andromeda.Hardware.Parameter

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS

import Control.Monad.Free
import Prelude hiding (read)
import Unsafe.Coerce

type HardwareName = BS.ByteString

data DeviceClass
    = Sensors
    | Ventiles
  deriving (Show, Read, Eq)

data DeviceDescription = DeviceDescription
    { _deviceClass :: DeviceClass
    , _deviceGuid :: BS.ByteString
    , _deviceName :: HardwareName
    , _deviceManufacturer :: BS.ByteString }
  deriving (Show, Read, Eq)


device = DeviceDescription


