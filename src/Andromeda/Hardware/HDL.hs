{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Andromeda.Hardware.HDL where

import Andromeda.Common
import Andromeda.Calculations

import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Description

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Free
import Data.Typeable

data HdlItem a = Sensor DeviceDescription HardwareName Par a
  deriving (Functor)

type Hdl a = Free HdlItem a

sensor :: DeviceDescription -> HardwareName -> Par -> Hdl ()
sensor dd n p = liftF (Sensor dd n p ())



