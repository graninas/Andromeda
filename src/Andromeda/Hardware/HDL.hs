{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Hardware.HDL where

import Andromeda.Common
import Andromeda.Calculations
import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Description

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Free

type DeviceIndexDef = BS.ByteString

-- | Convinient language for defining sensors and other devices.
data HdlItem a = SensorDef DeviceDescription DeviceIndexDef Par a
               | RtuDef DeviceDescription DeviceIndexDef a
  deriving (Functor)

-- | Free monad Hardware Definition Language.
-- By this definition a real device in hardware network should be composed.
type Hdl a = Free HdlItem a

sensor :: DeviceDescription -> DeviceIndexDef -> Par -> Hdl ()
sensor dd idx p = liftF (SensorDef dd idx p ())

rtu :: DeviceDescription -> DeviceIndexDef -> Hdl ()
rtu dd idx = liftF (RtuDef dd idx ())

