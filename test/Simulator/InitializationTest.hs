{-# LANGUAGE ExistentialQuantification #-}
module Simulator.InitializationTest where

import Lib
import Andromeda
import TestCommon

import qualified Data.Map as M
import Control.Concurrent
import Control.Monad.State as S
import Control.Monad.Free

data NoiseGenerator = forall a. NoiseGenerator (a -> a)

data In = SetNoiseGenerator ValueSource NoiseGenerator
data Out = Out String
  deriving (Show, Eq)
  
type SimulatorPipe = Pipe In Out

data Node = SensorNode Par
          | ControllerNode

type DevicesState = M.Map String Node
type NetworkScheme = M.Map String String
data SimulationModel = SimulationModel DevicesState NetworkScheme

emptyModel = SimulationModel M.empty M.empty

mkKey a b = show (a, b)

compileHdl :: PhysicalAddress -> Hdl () -> State SimulationModel ()
compileHdl pa (Pure _)   = return ()
compileHdl pa (Free def) = compile' pa def
  where
      compile' pa (SensorDef compDef compIdx par next) = do
          (SimulationModel ds ns) <- S.get
          let key = mkKey pa compIdx
          let ds' = M.insert key (SensorNode par) ds
          put (SimulationModel ds' ns)
          compileHdl pa next
      compile' pa (ControllerDef compDef compIdx next) = do
          (SimulationModel ds ns) <- S.get
          let key = mkKey pa compIdx
          let ds' = M.insert key ControllerNode ds
          put (SimulationModel ds' ns)
          compileHdl pa next
          
compileHndl :: Hndl () -> State SimulationModel ()
compileHndl (Pure _)   = return ()
compileHndl (Free def) = compile' def
  where
      compile' (RemoteDeviceDef pa hdl descr next) = do
          compileHdl pa hdl
          compileHndl $ next $ mkInterface pa
      compile' (TerminalUnitDef pa hdl descr next) = do
          compileHndl $ next $ mkInterface pa
      compile' (LogicControlDef pa descr next) = do
          compileHndl $ next $ mkInterface pa
      compile' (ConnectionDef interfaces descr next) = do
          compileHndl next

process req simModel = return (Out "initialized", simModel)

-- TODO: make simulation work in State monad or even in STM monad.
simulation pipe simModel = do
    req <- getRequest pipe
    (resp, simModel') <- process req simModel
    sendResponse pipe resp
    simulation pipe simModel'

gen1 = NoiseGenerator (\(Measurement (FloatValue v)) -> toKelvin (v+1.0))

compileSimModel :: Hndl () -> SimulationModel
compileSimModel net = let compiler = compileHndl net
                      in execState compiler emptyModel

test = do
    let simModel = compileSimModel networkDef
    
    pipe <- createPipe :: IO SimulatorPipe
    thrId <- forkIO $ simulation pipe simModel

    resp <- sendRequest pipe (SetNoiseGenerator boostersNozzle1T gen1)
    if (resp == Out "initialized") then print "passed." else print "failed."
    
    killThread thrId
    print "Simulation done."