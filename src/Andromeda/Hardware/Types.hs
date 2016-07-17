module Andromeda.Hardware.Types where

import qualified Data.ByteString.Char8 as BS

type PhysicalAddress = BS.ByteString
type ComponentIndex = BS.ByteString
type DeviceObjectIndex = BS.ByteString

type DeviceIndex = (DeviceObjectIndex, ComponentIndex)


