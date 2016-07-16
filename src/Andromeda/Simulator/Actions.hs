{-# LANGUAGE ExistentialQuantification #-}
module Andromeda.Simulator.Actions where

import Andromeda.Common
import Andromeda.Hardware
import Andromeda.Calculations
import Andromeda.Simulator.SimulationModel

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Maybe

floatIncrementGen = NoiseGenerator (\(Measurement (FloatValue v)) -> Measurement . FloatValue $ (v+1.0))

getSensorNode :: ComponentInstanceIndex -> SimState SensorNode
getSensorNode idx = do
    mbSensor <- use $ sensorsTable . at idx
    assert (isJust mbSensor) "Sensor not found" idx
    return $ fromJust mbSensor


setValueGenerator :: ComponentInstanceIndex -> ValueGenerator -> SimState ()
setValueGenerator idx g = do
    sensor <- getSensorNode idx
    let setValueGen tv g = liftIO $ atomically $ writeTVar tv g
    setValueGen (sensor ^. valueGenerator) g

setEnabled tv = liftIO $ atomically $ writeTVar tv True
          
runNetwork :: SimState ()
runNetwork = do
    m <- use $ sensorsTable
    let tvs = m ^.. traverse . producing
    mapM_ setEnabled tvs
    
getValueSource :: ComponentInstanceIndex -> SimState ValueSource
getValueSource idx = do
    sensor <- getSensorNode idx
    return $ sensor ^. valueSource