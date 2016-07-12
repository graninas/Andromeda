{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Andromeda.Simulator.Simulation
    ( compileSimModel
    , simulation
    )where

import Andromeda.Hardware hiding (sensors)
import Andromeda.Common

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Control.Monad.State as S
import Control.Monad.Free
import Control.Service.Remote
import Control.Lens
import Data.Maybe

-- TODO: use Device as Node?
data ControllerNode = ControllerNode

data SensorNode = SensorNode
    { _par :: Par
    }

type SensorsTable = M.Map ComponentInstanceIndex SensorNode
type ControllersTable = M.Map ComponentInstanceIndex ControllerNode
type NetworkScheme = M.Map String String

data SimulationModel = SimulationModel
    { _sensors :: SensorsTable
    , _controllers :: ControllersTable
    , _network :: NetworkScheme
    }

data CompilerState = CompilerState
    { _simulationModel :: SimulationModel
    , _componentAddress :: PhysicalAddress
    }
    
makeLenses ''SimulationModel
makeLenses ''CompilerState

type SimCompilerState = State CompilerState

assertNoSensor key = do
    mbS <- use (simulationModel . sensors . at key)
    assert (isNothing mbS) "Sensor exist" key

assertNoController key = do
    mbC <- use (simulationModel . controllers . at key)
    assert (isNothing mbC) "Controller exist" key
    
instance HdlInterpreter SimCompilerState where
   onSensorDef cd ci p = do
       pa <- use componentAddress
       assert (not $ BS.null pa) "ComponentAddress is null." ""
       let key = (pa, ci)
       assertNoSensor key
       (simulationModel . sensors . at key) %= (\_ -> Just (SensorNode p))
   onControllerDef cd ci = do
       pa <- use componentAddress
       assert (not $ BS.null pa) "ComponentAddress is null." ""
       let key = (pa, ci)
       assertNoController key
       (simulationModel . controllers . at key) %= (\_ -> Just ControllerNode)

instance HndlInterpreter SimCompilerState where
   onRemoteDeviceDef pa hdl d = do
       componentAddress .= pa
       interpretHdl hdl
       return $ mkInterface pa
   onTerminalUnitDef pa hdl d = do
       componentAddress .= pa
       interpretHdl hdl
       return $ mkInterface pa
   onLogicControlDef pa d = return $ mkInterface pa
   onConnectionDef is d = return ()

emptySimModel = SimulationModel M.empty M.empty M.empty
emptyCompilerState = CompilerState emptySimModel BS.empty

---- public interface:

compileSimModel :: Hndl () -> SimulationModel
compileSimModel hndl = 
    let compiler = interpretHndl hndl
        (CompilerState m _) = execState compiler emptyCompilerState
    in m
    
-- TODO: make simulation work in State monad or even in STM monad.
simulation pipe process simModel = do
    req <- getRequest pipe
    (resp, simModel') <- process req simModel
    sendResponse pipe resp
    simulation pipe process simModel'