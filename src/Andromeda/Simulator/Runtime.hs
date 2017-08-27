module Andromeda.Simulator.Runtime where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Lens
import Data.List as L (nub)
import Data.Maybe
import Data.Traversable as T (mapM)

import Control.Service.Remote

import Andromeda.Simulator.Types
import Andromeda.Simulator.Actions
import Andromeda.Simulator.Internal.SimulationCompiler
import Andromeda.Types.Hardware

-- TODO
process :: SimulatorProcess
process (SimAction act) = act >> return Ok
-- process GetDevices = OutDevices <$> getDeviceDefs
-- process GetHardwareHandle = OutHardwareHandle <$> getHardwareHandle
process (GetValueSource idx) = OutValueSource <$> getValueSource idx
process _ = return Ok   -- TODO

runNetworkAct = SimAction runNetwork
setGen1Act idx = SimAction $ setValueGenerator idx floatIncrementGen
setGen2Act idx = SimAction $ setValueGenerator idx floatDecrementGen

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
    unless prod retry
    processValueSource prod vsTVar vgTVar
  threadDelay (1000 * 10) -- 10 ms

forkSensorWorker :: SensorNode -> IO ThreadId
forkSensorWorker node = forkIO $ forever $ sensorWorker node

startSensorsSimulation :: SensorsModel -> IO SensorsHandles
startSensorsSimulation = T.mapM forkSensorWorker

stopSensorWorker :: ThreadId -> IO ()
stopSensorWorker = killThread

stopSensorsSimulation :: SensorsHandles -> IO ()
stopSensorsSimulation handles = void $ T.mapM stopSensorWorker handles

-- TODO: Tail recursive
simulationWorker :: SimulatorPipe -> SimulatorProcess -> SimState ()
simulationWorker pipe process = do
  req <- liftIO $ getRequest pipe
  resp <- process req
  liftIO $ sendResponse pipe resp
  simulationWorker pipe process

startSimulation pipe process simModel = do
  sensorsHandlers <- startSensorsSimulation (simModel ^. sensorsModel)
  simHandle <- forkIO $ void $ S.execStateT (simulationWorker pipe process) simModel
  return $ SimulationHandle simModel simHandle sensorsHandlers

stopSimulation (SimulationHandle simModel simHandle sensorsHandles) = do
  stopSensorsSimulation sensorsHandles
  killThread simHandle

-- TODO
runSimulation (SimulatorRuntime handleVar pipe simModel) = do
  h <- startSimulation pipe process simModel
  -- r1 <- sendRequest pipe (setGen1Act boostersNozzle1T)
  -- r2 <- sendRequest pipe (setGen2Act boostersNozzle2T)
  -- let isOk = L.nub [r1, r2] == [Ok]
  -- if isOk then putMVar handleVar h >> print "Simulation started."
  --         else print "Simulation failed."
  -- return isOk
  return True

terminateSimulation (SimulatorRuntime handleVar pipe simModel) = do
  h <- takeMVar handleVar
  stopSimulation h
  print "Simulation stopped."

makeSimulatorRuntime networkDef = do
  simModel <- compileSimModel networkDef
  pipe <- createPipe :: IO SimulatorPipe
  h <- newEmptyMVar
  return $ SimulatorRuntime h pipe simModel

getDevices (SimulatorRuntime handleVar pipe simModel) = do
  ds <- sendRequest pipe GetDevices
  return $ outDevices ds
