{-# LANGUAGE ExistentialQuantification #-}
module Simulator.InitializationTest where

import Lib
import Andromeda
import TestCommon

import qualified Data.Map as M
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Free

data NoiseGenerator = forall a. NoiseGenerator (a -> a)

data In = SetNoiseGenerator ValueSource NoiseGenerator
data Out = Out
  deriving (Show)
  
type SimulatorPipe = Pipe In Out
gen1 = NoiseGenerator (\(Measurement (FloatValue v)) -> toKelvin (v+1.0))

data Node = ValueSourceNode ValueSource
data NodeState = NodeState

type DevicesState = M.Map Node NodeState
type NetworkScheme = M.Map String String
data SimulationModel = SimulationModel DevicesState NetworkScheme

emptyModel = SimulationModel M.empty M.empty

compileNetwork :: Hndl () -> State SimulationModel ()
compileNetwork = compile'
  where
      compile' (Pure _) = return ()
      compile' (Free (RemoteDeviceDef pa hdl descr next)) = return ()
          -- TODO

runSimulation pipe _ = print "Simulator under construction." >> sendResponse pipe Out
{-
data HndlItem a = RemoteDeviceDef PhysicalAddress (Hdl ()) Description (Interface -> a)
                | TerminalUnitDef PhysicalAddress (Hdl ()) Description (Interface -> a)
                | LogicControlDef PhysicalAddress Description (Interface -> a)
                | ConnectionDef [Interface] Description a
  deriving (Functor)

data Component a = SensorDef ComponentDef ComponentIndex Par a
                 | ControllerDef ComponentDef ComponentIndex a
  deriving (Functor)

-}

runSim pipe net = do
    let simModel = runState (compileNetwork net) emptyModel
    runSimulation pipe simModel

test = do
    
    pipe <- createPipe :: IO SimulatorPipe
    thrId <- forkIO $ runSim pipe networkDef
    
    sendRequest pipe (SetNoiseGenerator boostersNozzle1T gen1) >>= print
    
    killThread thrId
    print "Simulation done."