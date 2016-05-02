{-# LANGUAGE FlexibleContexts #-}
module Andromeda.Hardware.Hardware (
    HardwareInterpreter(..),
    interpret,
    Hardware,
    blankHardware,
    addSensor,
    addRtu,
    readParameter
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

type DeviceIndex = BS.ByteString

-- | Device instance.
-- Abstract data type.
data Device = Sensor DeviceDescription Par
            | Rtu DeviceDescription
    deriving (Show, Eq)

-- | Pure instance of hardware.
-- Abstract data type.
newtype Hardware = HardwareImpl (M.Map DeviceIndex Device)
    deriving (Show, Eq)

blankHardware = HardwareImpl M.empty

readParameter :: DeviceIndex -> Hardware -> Maybe (Measurement tag)
readParameter idx (HardwareImpl ps) = case M.lookup idx ps of
    Nothing -> Nothing
    Just (Rtu _) -> Nothing
    -- TODO: remove hack with unsafeCoerce.
    Just (Sensor _ (Par v m a)) -> Just (unsafeCoerce $ convertAdmissible a undefined v)

addSensor :: DeviceIndex -> Par -> DeviceDescription -> Hardware -> Hardware
addSensor idx p dd (HardwareImpl m) = HardwareImpl $ M.insert idx (Sensor dd p) m

addRtu :: DeviceIndex -> DeviceDescription -> Hardware -> Hardware
addRtu idx dd (HardwareImpl m) = HardwareImpl $ M.insert idx (Rtu dd) m

class HardwareInterpreter m where
   onSensorDef :: MonadState Hardware m => DeviceDescription -> DeviceIndexDef -> Par -> m ()
   onRtuDef :: MonadState Hardware m => DeviceDescription -> DeviceIndexDef -> m ()
   onSensorDef _ _ _ = return ()
   onRtuDef _ _ = return ()

interpret :: (MonadState Hardware m, HardwareInterpreter m) => Hdl () -> m ()
interpret (Pure ())   = return ()
interpret (Free proc) = case proc of
    SensorDef dd idx p next -> do
        onSensorDef dd idx p
        modify (addSensor idx p dd)
        interpret next
    RtuDef dd idx next -> do
        onRtuDef dd idx
        modify (addRtu idx dd)
        interpret next
        
        
