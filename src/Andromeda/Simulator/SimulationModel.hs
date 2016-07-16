{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Andromeda.Simulator.SimulationModel where

import Andromeda.Common
import Andromeda.Hardware

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Maybe

data ValueGenerator = NoGenerator
                    | forall a. NoiseGenerator (a -> a)

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
    
type SimState = S.StateT SimulationModel IO
type Process req resp = req -> SimState resp

makeLenses ''SensorNode
makeLenses ''SimulationModel

emptySimModel = SimulationModel M.empty M.empty M.empty

noGenerator = NoGenerator

