{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Types.Language.Hardware.HDL where

import Control.Monad.Free

import Andromeda.Types.Hardware.Types
import Andromeda.Types.Hardware.Parameter
import Andromeda.Types.Hardware.Component

-- | Convinient language for defining sensors and other components.
-- TODO: ControllerDef dublicates TerminalUnit?
-- Maybe several controllers in one device. ComponentIndex is the selector.
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
