{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Andromeda.Simulator.Internal.SimulationCompiler where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Maybe

import Andromeda.Types.Hardware
import Andromeda.Types.Language.Hardware
import Andromeda.Simulator.Types
import Andromeda.Utils.Assert

data ComposingDevice = ComposingDevice
  { _composingSensors :: M.Map ComponentIndex SensorNode
  , _composingController :: Maybe (ComponentIndex, ControllerNode)
  }

data CompilerState = CompilerState
  { _simulationModel :: SimulationModel
  , _composingDevice :: ComposingDevice
  }

makeLenses ''ComposingDevice
makeLenses ''CompilerState

type SimCompilerState = S.StateT CompilerState IO

emptyComposingDevice = ComposingDevice M.empty Nothing
emptyCompilerState = CompilerState emptySimModel emptyComposingDevice

assertNoSensor idx = do
  mbS <- use (composingDevice . composingSensors . at idx)
  assert (isNothing mbS) "Sensor exist" idx

assertNoController idx = do
  mbC <- use $ composingDevice . composingController
  assert (isNothing mbC) "Controller exist" idx

mkDefaultSensorNode p = do
  tvP <- liftIO $ newTVarIO p
  tvG <- liftIO $ newTVarIO NoGenerator
  tvProd <- liftIO $ newTVarIO False
  return $ SensorNode tvP tvG tvProd

mkDefaultControllerNode = return ControllerNode

instance HdlInterpreter SimCompilerState where
  onSensorDef compDef compIdx par = do
    assertNoSensor compIdx
    sn <- mkDefaultSensorNode par
    composingDevice . composingSensors . at compIdx %= const (Just sn)
  onControllerDef compDef compIdx = do
    assertNoController compIdx
    cn <- mkDefaultControllerNode
    composingDevice . composingController .= Just (compIdx, cn)

instance HndlInterpreter SimCompilerState where
  onDeviceDef pa hdl d = do
    interpretHdl hdl
    m <- use $ composingDevice . composingSensors
    let m' = M.mapKeys (\compIdx -> (pa, compIdx)) m
    simulationModel . sensorsModel %= M.union m'
    composingDevice .= emptyComposingDevice
    return $ mkDeviceInterface pa
  onTerminalUnitDef pa d = return $ mkTerminalUnitInterface pa
  onLogicControlDef pa d = return $ mkInterface pa
  onLinkedDeviceDef (DeviceInterface rdi) (TerminalUnitInterface tui) = return ()
  onLinkDef interf tui = return ()

compileSimModel :: Hndl () -> IO SimulationModel
compileSimModel hndl = do
  let compiler = interpretHndl hndl
  (CompilerState m _) <- S.execStateT compiler emptyCompilerState
  return m
