{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Andromeda.Simulator.Simulation
    ( compileSimModel
    , simulation
    , startSimulation
    , stopSimulation
    ) where

import Andromeda.Hardware
import Andromeda.Common
import Andromeda.Simulator.SimulationModel

import Control.Service.Remote

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Maybe

data ComposingDevice = ComposingDevice
    { _composingSensors :: M.Map ComponentIndex SensorNode
    , _composingController :: Maybe (ComponentIndex, ControllerNode)
    }

data CompilerState = CompilerState
    { _simulationModel :: SimulationModel
    , _debugPrintEnabled :: Bool
    , _composingDevice :: ComposingDevice
    }
    
makeLenses ''ComposingDevice
makeLenses ''CompilerState

type SimCompilerState = S.StateT CompilerState IO

emptyComposingDevice = ComposingDevice M.empty Nothing
emptyCompilerState = CompilerState emptySimModel False emptyComposingDevice

assertNoSensor idx = do
    mbS <- use (composingDevice . composingSensors . at idx)
    assert (isNothing mbS) "Sensor exist" idx

assertNoController idx = do
    mbC <- use $ composingDevice . composingController
    assert (isNothing mbC) "Controller exist" idx
    
mkDefaultSensorNode p = do
    tvP <- liftIO $ newTVarIO p
    tvG <- liftIO $ newTVarIO noGenerator
    tvProd <- liftIO $ newTVarIO False
    return $ SensorNode tvP tvG tvProd
    
mkDefaultControllerNode = return ControllerNode

debugPrint v = do
    dp <- use debugPrintEnabled
    if dp then liftIO $ print v
          else return ()

instance HdlInterpreter SimCompilerState where
   onSensorDef compDef compIdx par = do
       debugPrint ("Compiling SensorDef", compIdx, compDef)
       assertNoSensor compIdx
       sn <- mkDefaultSensorNode par
       composingDevice . composingSensors . at compIdx %= (const $ Just sn)
   onControllerDef compDef compIdx = do
       debugPrint ("Compiling ControllerDef", compIdx, compDef)
       assertNoController compIdx
       cn <- mkDefaultControllerNode
       composingDevice . composingController .= (Just (compIdx, cn))

instance HndlInterpreter SimCompilerState where
   onRemoteDeviceDef hdl d = do
       debugPrint ("Compiling DeviceDef", d)
       devIdx@(deviceObjIdx, _) <- interpretHdl hdl
       m <- use $ composingDevice . composingSensors
       let m' = M.mapKeys (\compIdx -> (deviceObjIdx, compIdx)) m
       simulationModel . sensorsModel %= (M.union m')
       composingDevice .= emptyComposingDevice
       return $ mkRemoteDeviceInterface devIdx
   onTerminalUnitDef pa d = do
       debugPrint ("Compiling TerminalUnitDef", pa)
       return $ mkTerminalUnitInterface pa
   onLogicControlDef pa d = do
       debugPrint ("Compiling LogicControlDef", pa)
       return $ mkInterface pa
   onLinkedDeviceDef (RemoteDeviceInterface rdi) (TerminalUnitInterface tui) = do
       debugPrint ("Compiling LinkedDeviceDef", rdi, tui)
       return ()
   onLinkDef interf tui = do
       debugPrint "Compiling ConnectionDef"
       return ()

---- public interface:

compileSimModel :: Hndl () -> IO SimulationModel
compileSimModel hndl = do
    let compiler = interpretHndl hndl
    (CompilerState m _ _) <- S.execStateT compiler emptyCompilerState
    return m
    
-- TODO: make simulation work in State monad or even in STM monad.
simulation :: Pipe req resp -> Process req resp -> SimState ()
simulation pipe process = do
    req <- liftIO $ getRequest pipe
    resp <- process req
    liftIO $ sendResponse pipe resp
    simulation pipe process

startSimulation pipe process simModel = forkIO $ void $ S.execStateT (simulation pipe process) simModel
stopSimulation = killThread
