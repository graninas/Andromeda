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
   onSensorDef cd idx p    = modify (addSensor cd idx p)
   onControllerDef cd idx = modify (addController cd idx)

type DeviceIOState = StateT Device IO
   
instance HdlInterpreter DeviceIOState where
   onSensorDef cd idx p   = do
       lift $ print ("Sensor", cd, idx, p)
       modify (addSensor cd idx p)
   onControllerDef cd idx = do
       lift $ print ("Controller", cd, idx)
       modify (addController cd idx)

-- | Makes a real instanse of device defined by the language.
-- Operates in the State monad.
makeDevice :: Hdl () -> Device
makeDevice hdl = execState (interpretHdl hdl) blankDevice

readParameter :: ComponentIndex -> Device -> Maybe (Measurement tag)
readParameter idx h = do
    sensor <- getComponent idx h
    readSensor sensor

setParameterIO :: Typeable tag => DeviceIO -> ComponentIndex -> Measurement tag -> IO Bool
setParameterIO hrdwIO idx m = do
    d <- readDeviceIO hrdwIO
    let mbDevComp = do comp <- getComponent idx d
                       setSensor comp m
    case mbDevComp of
        Nothing -> return False
        _ -> do writeDeviceIO hrdwIO $ updateComponent mbDevComp idx d
                return True

        

-- | Makes a real instanse of device defined by the language.
-- Operates in the IO monad.
makeDeviceIO :: Hdl () -> IO DeviceIO
makeDeviceIO hdl = do
    hrdw <- execStateT (interpretHdl hdl) blankDevice
    newIORef hrdw

-- | Extracts device instance from IO container.
readDeviceIO = readIORef
writeDeviceIO = writeIORef

readParameterIO :: ComponentIndex -> DeviceIO -> IO (Maybe (Measurement tag))
readParameterIO idx hrdwIO  = do
    h <- readDeviceIO hrdwIO
    return $ readParameter idx h

