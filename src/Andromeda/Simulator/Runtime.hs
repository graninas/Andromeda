{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Andromeda.Simulator.Runtime
    ( simulation
    , startSimulation
    , stopSimulation
    , SimulationHandle
    ) where

import Andromeda.Hardware
import Andromeda.Common
import Andromeda.Simulator.Simulation

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Lens
import Data.Maybe
import Data.Traversable as T (mapM)

type SensorsHandles = M.Map ComponentInstanceIndex ThreadId
data SimulationHandle = SimulationHandle
    { _shModel :: SimulationModel
    , _shHandle :: ThreadId
    , _shSensorsHandles :: SensorsHandles
    }

generateValue NoGenerator vs = vs
generateValue (StepGenerator f) vs = f vs

processValueSource False _ _ = return ()
processValueSource True vsTVar vgTVar = do
    vs <- readTVar vsTVar
    vg <- readTVar vgTVar
    let vg' = generateValue vg vs
    writeTVar vsTVar vg'

-- TODO: fork threads when startSimulation is called.
sensorWorker sn@(SensorNode vsTVar vgTVar prodTVar) = do
    liftIO $ atomically $ do
        prod <- readTVar prodTVar
        when (not prod) retry
        processValueSource prod vsTVar vgTVar
    threadDelay (1000 * 10) -- 10 ms
    
forkSensorWorker :: SensorNode -> IO ThreadId
forkSensorWorker node = forkIO $ forever $ sensorWorker node

startSensorsSimulation :: SensorsModel -> IO SensorsHandles
startSensorsSimulation sensors = T.mapM forkSensorWorker sensors

stopSensorWorker :: ThreadId -> IO ()
stopSensorWorker threadId = killThread threadId

stopSensorsSimulation :: SensorsHandles -> IO ()
stopSensorsSimulation handles = void $ T.mapM stopSensorWorker handles

startSimulation pipe process simModel = do
    sensorsHandlers <- startSensorsSimulation (simModel ^. sensorsModel)
    simHandle <- forkIO $ void $ S.execStateT (simulation pipe process) simModel
    return $ SimulationHandle simModel simHandle sensorsHandlers
    
stopSimulation (SimulationHandle simModel simHandle sensorsHandles) = do
    stopSensorsSimulation sensorsHandles
    killThread simHandle
