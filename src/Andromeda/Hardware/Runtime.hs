{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Andromeda.Hardware.Runtime where

import Andromeda.Hardware.Types
import Andromeda.Hardware.Description
import Andromeda.Hardware.HDL
import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Device
import Andromeda.Calculations
import Andromeda.Common

import Data.IORef
import Data.Typeable
import Data.Data
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Free
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS

-- Incapsulated impure instance of Device.
type DeviceIO = IORef Device

-- Internal types.
type DeviceState = State Device
instance HdlInterpreter DeviceState where
   onSensorDef compDef compIdx p   = modify (addSensor compDef compIdx p)
   onControllerDef compDef compIdx = modify (addController compDef compIdx)

-- TODO: add debug print switch.
type DeviceIOState = StateT Device IO
instance HdlInterpreter DeviceIOState where
   onSensorDef compDef compIdx par = do
       --lift $ print ("Sensor", compDef, compIdx, p)
       modify (addSensor compDef compIdx par)
   onControllerDef compDef compIdx = do
       --lift $ print ("Controller", compDef, compIdx)
       modify (addController compDef compIdx)

-- | Makes a real instanse of device defined by the language.
-- Operates in the State monad.
makeDevice :: Hdl a -> Device
makeDevice hdl = execState (interpretHdl hdl) blankDevice

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
makeDeviceIO :: Hdl a -> IO DeviceIO
makeDeviceIO hdl = do
    dev <- execStateT (interpretHdl hdl) blankDevice
    newIORef dev

-- | Extracts device instance from IO container.
readDeviceIO = readIORef
writeDeviceIO = writeIORef

readParameterIO :: ComponentIndex -> DeviceIO -> IO (Maybe (Measurement tag))
readParameterIO compIdx hrdwIO  = do
    dev <- readDeviceIO hrdwIO
    return $ readParameter compIdx dev

