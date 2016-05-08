{-# LANGUAGE FlexibleContexts #-}
module Andromeda.Hardware.Device (
    DeviceInterpreter(..),
    interpret,
    Device,
    DeviceComponent,
    blankDevice,
    addSensor,
    addController,
    readSensor,
    getComponent
  ) where

import Andromeda.Hardware.Description
import Andromeda.Hardware.HDL
import Andromeda.Hardware.Parameter
import Andromeda.Calculations
import Andromeda.Common

import Control.Monad.State.Class
import Control.Monad.Free
import Unsafe.Coerce
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

-- | Component instance.
-- Abstract data type.
data DeviceComponent = Sensor Par Guid
                     | Controller Guid
    deriving (Show, Eq)

-- | Instance of Device.
-- Abstract data type.
newtype Device = DeviceImpl (M.Map ComponentIndex DeviceComponent)
    deriving (Show, Eq)

class DeviceInterpreter m where
   onSensorDef :: MonadState Device m => ComponentDef -> ComponentIndex -> Par -> m ()
   onControllerDef :: MonadState Device m => ComponentDef -> ComponentIndex -> m ()
   onSensorDef _ _ _ = return ()
   onControllerDef _ _ = return ()

blankDevice = DeviceImpl M.empty

addSensor :: ComponentIndex -> Par -> ComponentDef -> Device -> Device
addSensor idx p c (DeviceImpl m) = DeviceImpl $ M.insert idx (Sensor p (componentGuid c)) m

addController :: ComponentIndex -> ComponentDef -> Device -> Device
addController idx c (DeviceImpl m) = DeviceImpl $ M.insert idx (Controller (componentGuid c)) m

interpret :: (MonadState Device m, DeviceInterpreter m) => Hdl () -> m ()
interpret (Pure ())   = return ()
interpret (Free proc) = case proc of
    SensorDef c idx p next -> do
        onSensorDef c idx p
        modify (addSensor idx p c)
        interpret next
    ControllerDef c idx next -> do
        onControllerDef c idx
        modify (addController idx c)
        interpret next

getComponent :: ComponentIndex -> Device -> Maybe DeviceComponent
getComponent idx (DeviceImpl m) = M.lookup idx m

-- TODO: remove hack with unsafeCoerce.
readSensor :: DeviceComponent -> Maybe (Measurement tag)
readSensor (Sensor (Par v m a) _) = Just (unsafeCoerce $ convertAdmissible a undefined v)
readSensor _ = Nothing

setParameter :: DeviceComponent -> Measurement tag -> Maybe DeviceComponent
setParameter (Sensor p _) = undefined 
setParameter _ = undefined
