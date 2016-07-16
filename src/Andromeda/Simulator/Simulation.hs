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

data CompilerState = CompilerState
    { _simulationModel :: SimulationModel
    , _componentAddress :: PhysicalAddress
    , _debugPrintEnabled :: Bool
    }
    
makeLenses ''CompilerState

type SimCompilerState = S.StateT CompilerState IO

assertNoSensor key = do
    mbS <- use (simulationModel . sensorsTable . at key)
    assert (isNothing mbS) "Sensor exist" key

assertNoController key = do
    mbC <- use (simulationModel . controllersTable . at key)
    assert (isNothing mbC) "Controller exist" key
    
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
   onSensorDef cd ci p = do
       pa <- use componentAddress
       let key = (pa, ci)
       debugPrint ("Compiling SensorDef", key)
       assert (not $ BS.null pa) "ComponentAddress is null." ""
       assertNoSensor key
       sn <- mkDefaultSensorNode p
       (simulationModel . sensorsTable . at key) %= (const $ Just sn)
   onControllerDef cd ci = do
       pa <- use componentAddress
       let key = (pa, ci)
       debugPrint ("Compiling ControllerDef", key)
       assert (not $ BS.null pa) "ComponentAddress is null." ""
       assertNoController key
       cn <- mkDefaultControllerNode
       (simulationModel . controllersTable . at key) %= (const $ Just cn)

instance HndlInterpreter SimCompilerState where
   onRemoteDeviceDef pa hdl d = do
       debugPrint ("Compiling DeviceDef", pa)
       componentAddress .= pa
       interpretHdl hdl
       return $ mkInterface pa
   onTerminalUnitDef pa hdl d = do
       debugPrint ("Compiling TerminalUnitDef", pa)
       componentAddress .= pa
       interpretHdl hdl
       return $ mkInterface pa
   onLogicControlDef pa d = do
       debugPrint ("Compiling LogicControlDef", pa)
       return $ mkInterface pa
   onConnectionDef is d = do
       debugPrint "Compiling ConnectionDef"
       return ()

emptyCompilerState = CompilerState emptySimModel BS.empty False

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