{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Andromeda.Simulator.Simulation where

import Andromeda.Hardware
import Andromeda.Common
import Andromeda.Simulator.ValueGenerators

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

-- TODO: use Device as Node?
data ControllerNode = ControllerNode

data SensorNode = SensorNode
    { _par :: TVar Par
    , _valueGenerator :: TVar ValueGenerator
    , _producing :: TVar Bool
    }

type SensorsTable     = M.Map ComponentInstanceIndex SensorNode
type ControllersTable = M.Map ComponentInstanceIndex ControllerNode
type NetworkScheme    = M.Map String String

data SimulationModel = SimulationModel
    { _sensorsTable :: SensorsTable
    , _controllersTable :: ControllersTable
    , _network :: NetworkScheme
    }

data CompilerState = CompilerState
    { _simulationModel :: SimulationModel
    , _componentAddress :: PhysicalAddress
    }
    
makeLenses ''SensorNode
makeLenses ''SimulationModel
makeLenses ''CompilerState

type SimCompilerState = S.StateT CompilerState IO
type SimState = S.StateT SimulationModel IO
type Process req resp = req -> SimState resp

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
    
instance HdlInterpreter SimCompilerState where
   onSensorDef cd ci p = do
       pa <- use componentAddress
       let key = (pa, ci)
       liftIO $ print ("Compiling SensorDef", key)
       assert (not $ BS.null pa) "ComponentAddress is null." ""
       assertNoSensor key
       sn <- mkDefaultSensorNode p
       (simulationModel . sensorsTable . at key) %= (const $ Just sn)
   onControllerDef cd ci = do
       pa <- use componentAddress
       let key = (pa, ci)
       liftIO $ print ("Compiling ControllerDef", key)
       assert (not $ BS.null pa) "ComponentAddress is null." ""
       assertNoController key
       cn <- mkDefaultControllerNode
       (simulationModel . controllersTable . at key) %= (const $ Just cn)

instance HndlInterpreter SimCompilerState where
   onRemoteDeviceDef pa hdl d = do
       liftIO $ print ("Compiling DeviceDef", pa)
       componentAddress .= pa
       interpretHdl hdl
       return $ mkInterface pa
   onTerminalUnitDef pa hdl d = do
       liftIO $ print ("Compiling TerminalUnitDef", pa)
       componentAddress .= pa
       interpretHdl hdl
       return $ mkInterface pa
   onLogicControlDef pa d = do
       liftIO $ print ("Compiling LogicControlDef", pa)
       return $ mkInterface pa
   onConnectionDef is d = do
       liftIO $ print "Compiling ConnectionDef"
       return ()

emptySimModel = SimulationModel M.empty M.empty M.empty
emptyCompilerState = CompilerState emptySimModel BS.empty

setValueGen tv g = liftIO $ atomically $ writeTVar tv g

setValueGenerator :: ComponentInstanceIndex -> ValueGenerator -> SimState ()
setValueGenerator idx g = do
    mbSensor <- use $ sensorsTable . at idx
    assert (isJust mbSensor) "Sensor not found" idx
    setValueGen ((mbSensor ^?! _Just) ^. valueGenerator) g

---- public interface:

compileSimModel :: Hndl () -> IO SimulationModel
compileSimModel hndl = do
    let compiler = interpretHndl hndl
    (CompilerState m _) <- S.execStateT compiler emptyCompilerState
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