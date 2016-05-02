{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Hardware.HNDL where

import Andromeda.Common
import Andromeda.Calculations
import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Description
import Andromeda.Hardware.HDL

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Free
import Data.Typeable

newtype Interface = Interface PhysicalAddress

-- | Convinient language for defining devices in network.
data HndlItem a = RemoteDeviceDef PhysicalAddress (Hdl ()) Description (Interface -> a)
                | TerminalUnitDef PhysicalAddress (Hdl ()) Description (Interface -> a)
                | LogicControlDef PhysicalAddress Description (Interface -> a)
                | ConnectionDef [Interface] Description a
  deriving (Functor)

-- | Free monad Hardware Network Definition Language.
-- It's just a definition, but not a real hardware network.
type Hndl a = Free HndlItem a

remoteDevice :: PhysicalAddress -> Hdl () -> Description -> Hndl Interface
remoteDevice pa hdl d = liftF (RemoteDeviceDef pa hdl d id)

terminalUnit :: PhysicalAddress -> Hdl () -> Description -> Hndl Interface
terminalUnit pa hdl d = liftF (TerminalUnitDef pa hdl d id)

logicControl :: PhysicalAddress -> Description -> Hndl Interface
logicControl pa d = liftF (LogicControlDef pa d id)

connection :: [Interface] -> Description -> Hndl ()
connection ifs d = liftF (ConnectionDef ifs d ())
