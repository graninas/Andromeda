{-# LANGUAGE FlexibleContexts #-}
module Andromeda.Hardware.Device (
    DeviceInterpreter(..),
    interpret,
    Device,
    DeviceComponent,
    blankDevice,
    addSensor,
    addRtu,
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
data DeviceComponent = Sensor ComponentDef Par
                     | Rtu    ComponentDef
    deriving (Show, Eq)

-- | Instance of Device.
-- Abstract data type.
newtype Device = DeviceImpl (M.Map ComponentIndex DeviceComponent)
    deriving (Show, Eq)

class DeviceInterpreter m where
   onSensorDef :: MonadState Device m => ComponentDef -> ComponentIndex -> Par -> m ()
   onRtuDef :: MonadState Device m => ComponentDef -> ComponentIndex -> m ()
   onSensorDef _ _ _ = return ()
   onRtuDef _ _ = return ()

blankDevice = DeviceImpl M.empty

addSensor :: ComponentIndex -> Par -> ComponentDef -> Device -> Device
addSensor idx p c (DeviceImpl m) = DeviceImpl $ M.insert idx (Sensor c p) m

addRtu :: ComponentIndex -> ComponentDef -> Device -> Device
addRtu idx c (DeviceImpl m) = DeviceImpl $ M.insert idx (Rtu c) m

interpret :: (MonadState Device m, DeviceInterpreter m) => Hdl () -> m ()
interpret (Pure ())   = return ()
interpret (Free proc) = case proc of
    SensorDef c idx p next -> do
        onSensorDef c idx p
        modify (addSensor idx p c)
        interpret next
    RtuDef c idx next -> do
        onRtuDef c idx
        modify (addRtu idx c)
        interpret next

getComponent :: ComponentIndex -> Device -> Maybe DeviceComponent
getComponent idx (DeviceImpl m) = M.lookup idx m

-- TODO: remove hack with unsafeCoerce.
readSensor :: DeviceComponent -> Maybe (Measurement tag)
readSensor (Sensor _ (Par v m a)) = Just (unsafeCoerce $ convertAdmissible a undefined v)
readSensor _ = Nothing
        
