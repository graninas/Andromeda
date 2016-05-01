{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Hardware.HDL where

import Andromeda.Common
import Andromeda.Calculations
import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Description

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Free

-- | Convinient language for defining sensors and other devices.
data HdlItem a = Sensor DeviceDescription HardwareName Par a
               | Rtu DeviceDescription HardwareName a
  deriving (Functor)

-- | Free monad Hardware Definition Language.
-- It's just a definition, but not a real device in hardware network.
type Hdl a = Free HdlItem a

sensor :: DeviceDescription -> HardwareName -> Par -> Hdl ()
sensor dd n p = liftF (Sensor dd n p ())

rtu :: DeviceDescription -> HardwareName -> Hdl ()
rtu dd n = liftF (Rtu dd n ())


