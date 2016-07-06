{-# LANGUAGE FlexibleContexts #-}
module Andromeda.Hardware.Device (
    Device,
    DeviceComponent,
    blankDevice,
    addSensor,
    addController,
    readSensor,
    setSensor,
    getComponent,
    updateComponent,
    removeComponent
  ) where

import Andromeda.Hardware.Description
import Andromeda.Hardware.HDL
import Andromeda.Hardware.Parameter
import Andromeda.Calculations
import Andromeda.Common

import Control.Monad.State.Class
import Control.Monad.Free
import Data.Typeable
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

blankDevice = DeviceImpl M.empty
   
addSensor :: ComponentDef -> ComponentIndex -> Par -> Device -> Device
addSensor cd idx p (DeviceImpl m) = DeviceImpl $ M.insert idx (Sensor p (componentGuid cd)) m

addController :: ComponentDef -> ComponentIndex -> Device -> Device
addController cd idx (DeviceImpl m) = DeviceImpl $ M.insert idx (Controller (componentGuid cd)) m

removeComponent :: ComponentIndex -> Device -> Device
removeComponent idx (DeviceImpl d) = DeviceImpl $ M.delete idx d

updateComponent :: Maybe DeviceComponent -> ComponentIndex -> Device -> Device
updateComponent mbC idx (DeviceImpl d)  = DeviceImpl $ M.update (const mbC) idx d

getComponent :: ComponentIndex -> Device -> Maybe DeviceComponent
getComponent idx (DeviceImpl m) = M.lookup idx m

-- TODO: remove hack with unsafeCoerce.
readSensor :: DeviceComponent -> Maybe (Measurement tag)
readSensor (Sensor p _) = Just (toMeasurement p)
readSensor _ = Nothing

-- TODO: comparing of measurement units?
setSensor :: Typeable tag => DeviceComponent -> Measurement tag -> Maybe DeviceComponent
setSensor (Sensor p g) m = Just (Sensor (toPar m) g)
setSensor _ _ = error "Setting parameter to not a sensor." 
