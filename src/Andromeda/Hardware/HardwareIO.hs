{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Andromeda.Hardware.HardwareIO where

import Andromeda.Hardware.Description
import Andromeda.Hardware.HDL
import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Hardware
import Andromeda.Calculations
import Andromeda.Common

import Data.IORef
import Data.Typeable
import Data.Data
import Control.Monad.Free
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS

-- Incapsulated impure instance of Hardware.
type HardwareIO = IORef Hardware

-- Internal function making hardware by definiton.
interpretIO :: HardwareIO -> Hdl () -> IO ()
interpretIO iorh (Pure a)    = return a
interpretIO iorh (Free proc) = case proc of
    Sensor _ hName p next -> do
        modifyIORef iorh (addDevice hName p)
        interpretIO iorh next

-- | Makes a real instanse of hardware defined by the language.
-- Operates in the IO monad.
makeHardwareIO :: Hdl () -> IO HardwareIO
makeHardwareIO hdl = do
    iorh <- newIORef blankHardware
    interpretIO iorh hdl
    return iorh

-- | Extracts hardware instance from IO container.
readHardwareIO = readIORef


