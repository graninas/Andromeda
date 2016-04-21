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

type HardwareIO = IORef Hardware

interpret :: HardwareIO -> Hdl () -> IO ()
interpret iorh (Pure a) = return a
interpret iorh (Free proc) = case proc of
    Sensor dd hName p next -> do
        print hName -- TODO: debug
        print dd
        print p
        Hardware sensors <- readIORef iorh
        let newSs = M.insert hName p sensors
        writeIORef iorh (Hardware newSs)
        interpret iorh next

makeHardwareIO :: Hdl () -> IO HardwareIO
makeHardwareIO hdl = do
    iorh <- newIORef (blankHardware)
    interpret iorh hdl
    return iorh

readHardwareIO = readIORef
