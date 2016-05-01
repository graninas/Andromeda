{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Hardware.HNDL where

import Andromeda.Common
import Andromeda.Calculations
import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Description
import Andromeda.Hardware.HDL

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Free
import Data.Typeable

type PhysicalAddress = BS.ByteString
type NetworkName = BS.ByteString
    
-- | Convinient language for defining sensors and other devices.
data HndlItem a = Network NetworkName (Hndl ()) a
                | ConnectedDevice PhysicalAddress (Hdl ()) [Hdl ()] a
  deriving (Functor)

-- | Free monad Hardware Network Definition Language.
-- It's just a definition, but not a real hardware network.
type Hndl a = Free HndlItem a


network :: NetworkName -> Hndl () -> Hndl ()
network name hndl = liftF (Network name hndl ())

connectedDevice :: PhysicalAddress -> Hdl () -> [Hdl ()] -> Hndl ()
connectedDevice pa d rs = liftF (ConnectedDevice pa d rs ())

