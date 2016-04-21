{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Andromeda.Hardware.Interpreter where

import Andromeda.Hardware.Description
import Andromeda.Hardware.HDL
import Andromeda.Hardware.Parameter
import Andromeda.Calculations
import Andromeda.Common

import Data.IORef
import Data.Typeable
import Data.Data
import Control.Monad.Free
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS

newtype Hardware = Hardware (M.Map HardwareName Par)
    deriving (Show, Eq)
type HardwareObj = IORef Hardware

blankHardware = Hardware M.empty


interpret :: HardwareObj -> Hdl () -> IO ()
interpret iorh (Pure a) = return a
interpret iorh (Free proc) = case proc of
    Sensor dd hName p next -> do
        print hName
        print dd
        print p        
        Hardware sensors <- readIORef iorh
        let newSs = M.insert hName p sensors
        writeIORef iorh (Hardware newSs)
        interpret iorh next



--makeHardware :: Hdl () -> IO HardwareObj
makeHardware hdl = do
    iorh <- newIORef (blankHardware)
    interpret iorh hdl
    return iorh

