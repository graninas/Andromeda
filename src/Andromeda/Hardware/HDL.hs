{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Hardware.HDL where

import Andromeda.Common
import Andromeda.Calculations
import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Description

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Free

type ComponentIndex = BS.ByteString

-- | Convinient language for defining sensors and other devices.
data Component a = SensorDef ComponentDef ComponentIndex Par a
                 | ControllerDef ComponentDef ComponentIndex a
  deriving (Functor)

-- | Free monad Hardware Definition Language.
-- By this definition a real device in hardware network should be composed.
type Hdl a = Free Component a

sensor :: ComponentDef -> ComponentIndex -> Par -> Hdl ()
sensor c idx p = liftF (SensorDef c idx p ())

controller :: ComponentDef -> ComponentIndex -> Hdl ()
controller c idx = liftF (ControllerDef c idx ())

