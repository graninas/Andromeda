{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Andromeda
import Lib
import Assets.SpaceshipSample

import Graphics.QML as QML
import System.Directory
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Typeable
import Data.Proxy
import Control.Concurrent.MVar
import Control.Monad (when)

type SimulatorPipe = Pipe In Out

-- Can type families be used here?
    
data In = SimAction (SimState ())
        | GetDevices
        | GetValueSource ComponentInstanceIndex
        | Start ComponentInstanceIndex
        | Stop ComponentInstanceIndex

data Out = Ok
         | OutValueSource ValueSource
         | OutDevices { outDevices :: [Device] }
         
instance Eq Out where
    Ok == Ok = True
    _ == _ = False

-- dummy
getDevices = return
    [ makeDevice boostersDef "boosters"
    , makeDevice rotaryEngineDef "rotary engine" ]
    
process :: Process In Out
process (SimAction act) = act >> return Ok
process GetDevices = do
    ds <- getDevices
    return $ OutDevices ds
process (GetValueSource idx) = do
    v <- getValueSource idx
    return $ OutValueSource v

runNetworkAct = SimAction $ runNetwork
setGen1Act idx = SimAction $ setValueGenerator idx floatIncrementGen
setGen2Act idx = SimAction $ setValueGenerator idx floatDecrementGen
    
data ShellVM = ShellVM
    { _shellWorkspaceVM :: ObjRef WorkspaceVM
    , _shellWorkspaceFile :: T.Text
    } deriving (Typeable)

data WorkspaceVM = SimulatorWorkspaceVM
    { _workspaceSimHandle :: MVar SimulatorHandle
    , _workspaceSimPipe :: SimulatorPipe
    , _workspaceSimModel :: SimulationModel
    , _workspaceSimDevices :: MVar [ObjRef DeviceVM]
    } deriving (Typeable)

data DeviceVM = DeviceVM
    { _deviceName :: T.Text
    }
    
instance DefaultClass ShellVM where
    classMembers =
        [ defPropertyConst' "vmWorkspace" (return . _shellWorkspaceVM . fromObjRef)
        , defPropertyConst' "vmWorkspaceFile" (return . _shellWorkspaceFile . fromObjRef)
        ]

data DevicesChanged deriving Typeable

instance SignalKeyClass DevicesChanged where
    type SignalParams DevicesChanged = IO ()
    
instance DefaultClass WorkspaceVM where
  classMembers = [ defMethod' "vmToggleSimulation" toggleSim'
                 , defPropertySigRO "vmDevicesChanged" (Proxy :: Proxy DevicesChanged) getDevices']
    where
        toggleSim' :: ObjRef WorkspaceVM -> Bool -> IO ()
        toggleSim' objRef toggle = do
            let vm = fromObjRef objRef
            if toggle then toggleSimulationOn vm >> fireSignal (Proxy :: Proxy DevicesChanged) objRef
                      else toggleSimulationOff vm
        getDevices' :: ObjRef WorkspaceVM -> IO [ObjRef DeviceVM]
        getDevices' workspaceObjRef = 
            let (SimulatorWorkspaceVM _ _ _ mv) = fromObjRef workspaceObjRef
            in readMVar mv
            

instance DefaultClass DeviceVM where
  classMembers = [ defPropertyConst' "vmDeviceName" (return . _deviceName . fromObjRef) ]

toggleSimulationOn (SimulatorWorkspaceVM hVar pipe simModel mvDevices) = do
    h <- startSimulation pipe process simModel
    r1 <- sendRequest pipe (setGen1Act boostersNozzle1T)
    r2 <- sendRequest pipe (setGen2Act boostersNozzle2T)
    when (r1 /= Ok) $ error "Failed to set value gen to boostersNozzle1T."
    when (r2 /= Ok) $ error "Failed to set value gen to boostersNozzle2T."
    putMVar hVar h
    ds <- sendRequest pipe GetDevices
    dVms <- mapM createDeviceVM (outDevices ds)
    _ <- swapMVar mvDevices dVms
    print $ "Devices created: " ++ show (length dVms)
    print "Simulation started."
        
toggleSimulationOff (SimulatorWorkspaceVM hVar pipe simModel _) = do
    h <- takeMVar hVar
    stopSimulation h
    print "Simulation stopped."

createDeviceVM device = newObjectDC $ DeviceVM (T.pack $ getDeviceId device)
    
createShellVM (workspaceFile, workspaceModel) = do
    workspaceVM <- newObjectDC workspaceModel
    let shellVM = ShellVM workspaceVM workspaceFile
    newObjectDC shellVM
    
startUiApplication workspace = do
    let view = fileDocument "app/Views/ShellView.qml"
    viewModel <- createShellVM workspace

    runEngineLoop QML.defaultEngineConfig {
        initialDocument = view,
        contextObject = Just $ QML.anyObjRef viewModel
    }

makeSimulator = do
    simModel <- compileSimModel networkDef
    pipe <- createPipe :: IO SimulatorPipe
    h <- newEmptyMVar
    return (h, pipe, simModel)
        
main :: IO ()
main = do
    print "Andromeda Control Software, version 0.1"
    print "Loading sample spaceship network..."
    (simHandle, pipe, simModel) <- makeSimulator
    devices <- newMVar []
    let workspace = SimulatorWorkspaceVM simHandle pipe simModel devices
    let workspaceFile = T.pack $ "SimulatorWorkspaceView.qml"
    startUiApplication (workspaceFile, workspace)
