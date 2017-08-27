{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Andromeda.Types.Hardware.Device where

import Control.Monad.Free
import Control.Monad.State
import qualified Control.Monad.Trans.State as S
import Control.Lens
import Data.IORef
import Data.Typeable
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

import Andromeda.Types.Hardware.Types
import Andromeda.Types.Hardware.Component
import Andromeda.Types.Hardware.Parameter
import Andromeda.Types.Language.Hardware
import Andromeda.Types.Physics

-- | Component instance.
-- Abstract data type.
data DeviceComponent
  = SensorImpl ComponentIndex ComponentDef Par
  | ControllerImpl ComponentIndex ComponentDef
  deriving (Show, Eq)

type DeviceId = String

-- | Instance of Device.
-- This type may be used as database entity. With other types like this one it will be data model.
-- Abstract data type.
data Device
  = DeviceImpl DeviceId (M.Map ComponentIndex DeviceComponent)
  deriving (Show, Eq)

-- Encapsulated impure instance of Device.
type DeviceIO = IORef Device
-- Internal types.

blankDevice = DeviceImpl "" M.empty

addSensor :: ComponentDef -> ComponentIndex -> Par -> Device -> Device
addSensor cd idx par (DeviceImpl dId m) = DeviceImpl dId $ M.insert idx (SensorImpl idx cd par) m

addController :: ComponentDef -> ComponentIndex -> Device -> Device
addController cd idx (DeviceImpl dId m) = DeviceImpl dId $ M.insert idx (ControllerImpl idx cd) m

removeComponent :: ComponentIndex -> Device -> Device
removeComponent idx (DeviceImpl dId d) = DeviceImpl dId $ M.delete idx d

updateComponent :: Maybe DeviceComponent -> ComponentIndex -> Device -> Device
updateComponent mbC idx (DeviceImpl dId d) = DeviceImpl dId $ M.update (const mbC) idx d

getComponent :: ComponentIndex -> Device -> Maybe DeviceComponent
getComponent idx (DeviceImpl _ m) = M.lookup idx m

getComponents :: Device -> [DeviceComponent]
getComponents (DeviceImpl _ d) = M.elems d

getComponentClass :: DeviceComponent -> ComponentClass
getComponentClass (SensorImpl _ cd _) = cd ^. componentClass
getComponentClass (ControllerImpl _ cd) = cd ^. componentClass

-- TODO: remove hack with unsafeCoerce.
readSensor :: DeviceComponent -> Maybe (Measurement tag)
readSensor (SensorImpl _ _ p) = Just (toMeasurement p)
readSensor _ = Nothing

-- TODO: comparing of measurement units?
setSensor :: Typeable tag => DeviceComponent -> Measurement tag -> Maybe DeviceComponent
setSensor (SensorImpl idx cd p) m = Just (SensorImpl idx cd (toPar m))
setSensor _ _ = error "Setting parameter to not a sensor."

getDeviceId :: Device -> DeviceId
getDeviceId (DeviceImpl dId _) = dId

setDeviceId :: DeviceId -> Device -> Device
setDeviceId dId (DeviceImpl _ d) = DeviceImpl dId d

type DeviceState = State Device
instance HdlInterpreter DeviceState where
  onSensorDef compDef compIdx p   = modify (addSensor compDef compIdx p)
  onControllerDef compDef compIdx = modify (addController compDef compIdx)

type DeviceIOState = StateT Device IO
instance HdlInterpreter DeviceIOState where
  onSensorDef compDef compIdx par = modify (addSensor compDef compIdx par)
  onControllerDef compDef compIdx = modify (addController compDef compIdx)

-- | Makes a real instanse of device defined by the language.
-- Operates in the State monad.
makeDevice :: Hdl a -> DeviceId -> Device
makeDevice hdl dId = let d = execState (interpretHdl hdl) blankDevice
                     in setDeviceId dId d

readParameter :: ComponentIndex -> Device -> Maybe (Measurement tag)
readParameter compIdx dev = do
  sensor <- getComponent compIdx dev
  readSensor sensor

setParameterIO :: Typeable tag => DeviceIO -> ComponentIndex -> Measurement tag -> IO Bool
setParameterIO devIO compIdx m = do
  dev <- readDeviceIO devIO
  let mbDevComp = do comp <- getComponent compIdx dev
                     setSensor comp m
  case mbDevComp of
    Nothing -> return False
    _ -> do writeDeviceIO devIO $ updateComponent mbDevComp compIdx dev
            return True

-- | Makes a real instanse of device defined by the language.
-- Operates in the IO monad.
makeDeviceIO :: Hdl a -> DeviceId -> IO DeviceIO
makeDeviceIO hdl dId = do
  dev <- execStateT (interpretHdl hdl) blankDevice
  newIORef (setDeviceId dId dev)

-- | Extracts device instance from IO container.
readDeviceIO = readIORef
writeDeviceIO = writeIORef

readParameterIO :: ComponentIndex -> DeviceIO -> IO (Maybe (Measurement tag))
readParameterIO compIdx hrdwIO  = do
  dev <- readDeviceIO hrdwIO
  return $ readParameter compIdx dev
