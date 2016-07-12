{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Hardware.HDL where

import Andromeda.Common
import Andromeda.Calculations
import Andromeda.Hardware.Types
import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Description

import Control.Monad.Free

-- | Convinient language for defining sensors and other devices.
data Component a = SensorDef ComponentDef ComponentIndex Par a
                 | ControllerDef ComponentDef ComponentIndex a
  deriving (Functor)

-- | Free monad Hardware Definition Language.
-- By this definition a real device in hardware network should be composed.
type Hdl a = Free Component a

class HdlInterpreter m where
   onSensorDef :: Monad m => ComponentDef -> ComponentIndex -> Par -> m ()
   onControllerDef :: Monad m => ComponentDef -> ComponentIndex -> m ()
   
interpretHdl :: (Monad m, HdlInterpreter m) => Hdl a -> m a
interpretHdl (Pure a) = return a
interpretHdl (Free proc) = case proc of
    SensorDef cd idx p next -> do
        onSensorDef cd idx p
        interpretHdl next
    ControllerDef cd idx next -> do
        onControllerDef cd idx
        interpretHdl next

sensor :: ComponentDef -> ComponentIndex -> Par -> Hdl ()
sensor c idx p = liftF (SensorDef c idx p ())

controller :: ComponentDef -> ComponentIndex -> Hdl ()
controller c idx = liftF (ControllerDef c idx ())