{-# LANGUAGE TemplateHaskell #-}

module Andromeda.Simulator.Types where

import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Lens

import Andromeda.Types.Hardware
import Andromeda.Runtime.Hardware.Handle

data ValueGenerator
  = NoGenerator
  | StepGenerator (Par -> Par)

-- TODO: use Device as Node?
data ControllerNode = ControllerNode

type ValueSource = TVar Par
type ValueSources = M.Map ComponentInstanceIndex ValueSource

data SensorNode = SensorNode
  { _valueSource :: ValueSource
  , _valueGenerator :: TVar ValueGenerator
  , _producing :: TVar Bool
  }

type SensorsModel = M.Map ComponentInstanceIndex SensorNode
type ControllersModel = M.Map ComponentInstanceIndex ControllerNode
type NetworkScheme = M.Map String String -- TODO

data SimulationModel = SimulationModel
  { _sensorsModel :: SensorsModel
  , _controllersModel :: ControllersModel
  , _networkScheme :: NetworkScheme
  }

type SimState = S.StateT SimulationModel IO
type Process req resp = req -> SimState resp

type SensorsHandles = M.Map ComponentInstanceIndex ThreadId
data SimulationHandle = SimulationHandle
  { _shModel :: SimulationModel
  , _shHandle :: ThreadId
  , _shSensorsHandles :: SensorsHandles
  }

makeLenses ''SensorNode
makeLenses ''SimulationModel

emptySimModel = SimulationModel M.empty M.empty M.empty
