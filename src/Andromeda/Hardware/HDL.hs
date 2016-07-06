{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Hardware.HDL where

import Andromeda.Common
import Andromeda.Calculations
import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Description

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

class HdlInterpreter m where
   onSensorDef :: Monad m => ComponentDef -> ComponentIndex -> Par -> m ()
   onControllerDef :: Monad m => ComponentDef -> ComponentIndex -> m ()
   
interpretHdl :: (Monad m, HdlInterpreter m) => Hdl a -> m a
interpretHdl (Pure a)   = return a
interpretHdl (Free proc) = case proc of
    SensorDef cd idx p next -> do
        onSensorDef cd idx p
        interpretHdl next
    ControllerDef cd idx next -> do
        onControllerDef cd idx
        interpretHdl next