{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Types.Language.Hardware.HNDL where

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Free
import Data.Typeable

import Andromeda.Types.Hardware.Types
import Andromeda.Types.Language.Hardware.HDL

-- TODO: does not enforce any type-level checks that the HDL interpreter will return index of controller.
newtype DeviceInterface = DeviceInterface PhysicalAddress
  deriving (Show, Eq)
newtype TerminalUnitInterface = TerminalUnitInterface PhysicalAddress
  deriving (Show, Eq)
newtype Interface = Interface PhysicalAddress
  deriving (Show, Eq)

-- | Convenient language for defining devices in network.
data NetworkComponent a
  = DeviceDef PhysicalAddress (Hdl ()) Description (DeviceInterface -> a)
  | TerminalUnitDef PhysicalAddress Description (TerminalUnitInterface -> a)
  | LogicControlDef PhysicalAddress Description (Interface -> a) -- TODO: seems unuseful. Duplicates TerminalUnitDef.
  | LinkedDeviceDef DeviceInterface TerminalUnitInterface a
  | LinkDef Interface TerminalUnitInterface a
  deriving (Functor)

-- | Free monad Hardware Network Definition Language.
-- It's just a definition, but not a real hardware network.
type Hndl a = Free NetworkComponent a

class HndlInterpreter m where
   onDeviceDef :: Monad m => PhysicalAddress -> Hdl () -> Description -> m DeviceInterface
   onTerminalUnitDef :: Monad m => PhysicalAddress -> Description -> m TerminalUnitInterface
   onLogicControlDef :: Monad m => PhysicalAddress -> Description -> m Interface
   onLinkedDeviceDef :: Monad m => DeviceInterface -> TerminalUnitInterface -> m ()
   onLinkDef     :: Monad m => Interface -> TerminalUnitInterface -> m ()

mkInterface = Interface
mkDeviceInterface = DeviceInterface
mkTerminalUnitInterface = TerminalUnitInterface

interpretHndl :: (Monad m, HndlInterpreter m) => Hndl a -> m a
interpretHndl (Pure a) = return a
interpretHndl (Free p) = case p of
  DeviceDef pa hdl d nextF -> do
    i <- onDeviceDef pa hdl d
    interpretHndl $ nextF i
  TerminalUnitDef pa d nextF -> do
    i <- onTerminalUnitDef pa d
    interpretHndl $ nextF i
  LogicControlDef pa d nextF -> do
    i <- onLogicControlDef pa d
    interpretHndl $ nextF i
  LinkedDeviceDef rdi tui next -> do
    onLinkedDeviceDef rdi tui
    interpretHndl next
  LinkDef i tui next -> do
    onLinkDef i tui
    interpretHndl next

remoteDevice :: PhysicalAddress -> Hdl () -> Description -> Hndl DeviceInterface
remoteDevice pa hdl d = liftF $ DeviceDef pa hdl d id

terminalUnit :: PhysicalAddress -> Description -> Hndl TerminalUnitInterface
terminalUnit pa d = liftF $ TerminalUnitDef pa d id

logicControl :: PhysicalAddress -> Description -> Hndl Interface
logicControl pa d = liftF $ LogicControlDef pa d id

linkedDevice :: DeviceInterface -> TerminalUnitInterface -> Hndl ()
linkedDevice rdi tui = liftF $ LinkedDeviceDef rdi tui ()

link :: Interface -> TerminalUnitInterface -> Hndl ()
link i tui = liftF $ LinkDef i tui ()
