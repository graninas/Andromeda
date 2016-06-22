module Andromeda.Simulator.Simulation where

import Andromeda.Hardware

import qualified Data.Map as M
import Control.Monad.State as S
import Control.Monad.Free
import Control.Service.Remote

-- TODO: use Device as Node?
data Node = SensorNode Par
          | ControllerNode

type DevicesState = M.Map String Node
type NetworkScheme = M.Map String String
data SimulationModel = SimulationModel DevicesState NetworkScheme

-- TODO: make simulation work in State monad or even in STM monad.
simulation pipe process simModel = do
    req <- getRequest pipe
    (resp, simModel') <- process req simModel
    sendResponse pipe resp
    simulation pipe process simModel'

compileHdl :: PhysicalAddress -> Hdl () -> State SimulationModel ()
compileHdl pa (Pure _)   = return ()
compileHdl pa (Free def) = compile' pa def
  where
      mkKey a b = show (a, b)
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

-- TODO: finish compiler.
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
    
compileSimModel :: Hndl () -> SimulationModel
compileSimModel net = let compiler = compileHndl net
                      in execState compiler emptySimModel
    
emptySimModel = SimulationModel M.empty M.empty