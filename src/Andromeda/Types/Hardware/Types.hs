module Andromeda.Types.Hardware.Types where

import qualified Data.ByteString.Char8 as BS

type PhysicalAddress = BS.ByteString
type ComponentIndex = BS.ByteString
type ComponentInstanceIndex = (PhysicalAddress, ComponentIndex)

type HardwareName = BS.ByteString
type Description = BS.ByteString

class ControllerLike a where
  getPhysicalAddress :: a -> PhysicalAddress

newtype Controller = Controller BS.ByteString
  deriving (Show, Read, Eq)

instance ControllerLike Controller where
  getPhysicalAddress (Controller addr) = addr
