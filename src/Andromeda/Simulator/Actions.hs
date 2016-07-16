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

setValueGenerator :: ComponentInstanceIndex -> ValueGenerator -> SimState ()
setValueGenerator idx g = do
    mbSensor <- use $ sensorsTable . at idx
    assert (isJust mbSensor) "Sensor not found" idx
    let setValueGen tv g = liftIO $ atomically $ writeTVar tv g
    setValueGen ((mbSensor ^?! _Just) ^. valueGenerator) g