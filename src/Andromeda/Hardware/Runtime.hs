{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Andromeda.Hardware.Runtime where

import Andromeda.Hardware.Description
import Andromeda.Hardware.HDL
import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Hardware
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

-- Incapsulated impure instance of Hardware.
type HardwareIO = IORef Hardware

-- Internal types.
type HardwareState = State Hardware
instance HardwareInterpreter HardwareState where
   onSensorDef _ _ _ = return ()

type HardwareIOState = StateT Hardware IO
instance HardwareInterpreter HardwareIOState where
   onSensorDef dd idx p = lift $ putStrLn $ 
    "Descr: " ++ show dd ++
    " Idx: "  ++ BS.unpack idx ++
    " Par: "  ++ show p
   onRtuDef dd idx = lift $ putStrLn $ 
    "Descr: " ++ show dd ++
    " Idx: "  ++ BS.unpack idx

{-
interpret :: Hdl () -> State Hardware ()
interpret (Pure a)    = return a
interpret (Free proc) = case proc of
    SensorDef dd idx p next -> do
        modify (addSensor (deviceIndex idx) p dd)
        interpret next
    RtuDef dd idx -> do
        modify (addRtu (deviceIndex idx) dd)
        interpret next

-- Internal function making hardware by definiton.
interpretIO :: HardwareIO -> Hdl () -> IO ()
interpretIO iorh (Pure a)    = return a
interpretIO iorh (Free proc) = case proc of
    Sensor dd idx p next -> do
        modifyIORef iorh (addSensor (deviceIndex idx) p dd)
        interpretIO iorh next
    Rtu dd idx -> do
        modifyIORef iorh (addRtu (deviceIndex idx) dd)
        interpretIO iorh next
-}

-- | Makes a real instanse of hardware defined by the language.
-- Operates in the State monad.
makeHardware :: Hdl () -> Hardware
makeHardware hdl = execState (interpret hdl) blankHardware

-- | Makes a real instanse of hardware defined by the language.
-- Operates in the IO monad.
makeHardwareIO :: Hdl () -> IO HardwareIO
makeHardwareIO hdl = do
    hrdw <- execStateT (interpret hdl) blankHardware
    newIORef hrdw

-- | Extracts hardware instance from IO container.
readHardwareIO = readIORef

