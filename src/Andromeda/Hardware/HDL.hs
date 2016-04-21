{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
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
import Unsafe.Coerce

data HdlItem tag a = Sensor DeviceDescription HardwareName (Parameter tag) a
  deriving (Functor)

type HdlT tag a = Free (HdlItem tag) a
type Hdl a = forall tag. HdlT tag a

sensor :: DeviceDescription -> HardwareName -> Parameter tag -> HdlT tag ()
sensor dd n p = liftF (Sensor dd n p ())



