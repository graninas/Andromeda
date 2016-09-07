{-# LANGUAGE FlexibleContexts #-}
module Andromeda.Hardware.Device (
    Device,
    DeviceComponent,
    DeviceId,
    blankDevice,
    addSensor,
    addController,
    readSensor,
    setSensor,
    getComponent,
    updateComponent,
    removeComponent,
    getDeviceId,
    setDeviceId
  ) where

import Andromeda.Hardware.Types
import Andromeda.Hardware.Description
import Andromeda.Hardware.HDL
import Andromeda.Hardware.Parameter
import Andromeda.Calculations
import Andromeda.Common

import Control.Monad.State.Class
import Control.Monad.Free
import Control.Lens
import Data.Typeable
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

-- | Component instance.
-- Abstract data type.
data DeviceComponent = Sensor Par
                     | Controller
    deriving (Show, Eq)
    
type DeviceId = String

-- | Instance of Device.
-- This type may be used as database entity. With other types like this one it will be data model.
-- Abstract data type.
data Device = DeviceImpl DeviceId (M.Map ComponentIndex DeviceComponent)
    deriving (Show, Eq)

blankDevice = DeviceImpl "" M.empty

addSensor :: ComponentDef -> ComponentIndex -> Par -> Device -> Device
addSensor cd idx par (DeviceImpl dId m) = DeviceImpl dId $ M.insert idx (Sensor par) m

addController :: ComponentDef -> ComponentIndex -> Device -> Device
addController cd idx (DeviceImpl dId m) = DeviceImpl dId $ M.insert idx (Controller) m

removeComponent :: ComponentIndex -> Device -> Device
removeComponent idx (DeviceImpl dId d) = DeviceImpl dId $ M.delete idx d

updateComponent :: Maybe DeviceComponent -> ComponentIndex -> Device -> Device
updateComponent mbC idx (DeviceImpl dId d)  = DeviceImpl dId $ M.update (const mbC) idx d

getComponent :: ComponentIndex -> Device -> Maybe DeviceComponent
getComponent idx (DeviceImpl _ m) = M.lookup idx m

-- TODO: remove hack with unsafeCoerce.
readSensor :: DeviceComponent -> Maybe (Measurement tag)
readSensor (Sensor p) = Just (toMeasurement p)
readSensor _ = Nothing

-- TODO: comparing of measurement units?
setSensor :: Typeable tag => DeviceComponent -> Measurement tag -> Maybe DeviceComponent
setSensor (Sensor p) m = Just (Sensor (toPar m))
setSensor _ _ = error "Setting parameter to not a sensor." 

getDeviceId :: Device -> DeviceId
getDeviceId (DeviceImpl dId d) = dId

setDeviceId :: DeviceId -> Device -> Device
setDeviceId dId (DeviceImpl _ d) = DeviceImpl dId d
