module Main where

import Andromeda
import Lib
import Assets.SpaceshipSample

import Graphics.QML as QML
import System.Directory
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Control.Concurrent.MVar
import Control.Monad (when)

type SimulatorPipe = Pipe In Out

data Workspace = SimulatorWorkspace
    { workspaceView :: String 
    , workspaceSimPipe :: SimulatorPipe
    , workspaceSimModel :: SimulationModel
    }

data Out = Out String
         | OutValueSource ValueSource
    
data In = SimStateSimpleAction (SimState ())
        | GetValueSource ComponentInstanceIndex
        | Start ComponentInstanceIndex
        | Stop ComponentInstanceIndex

instance Eq Out where
    Out s1 == Out s2 = s1 == s2
    OutValueSource v1 == OutValueSource v2 = False
    _ == _ = False
        
ok = Out "OK."

process :: Process In Out
process (SimStateSimpleAction act) = act >> return ok
process (GetValueSource idx) = do
    v <- getValueSource idx
    return $ OutValueSource v
    
okOnSuccessAction act = SimStateSimpleAction act

runNetworkAct = okOnSuccessAction $ runNetwork
setGen1Act idx = okOnSuccessAction $ setValueGenerator idx floatIncrementGen
setGen2Act idx = okOnSuccessAction $ setValueGenerator idx floatDecrementGen

toggleSimulationOn pipe simModel ref = do
    simStopped <- isEmptyMVar ref
    if not simStopped
        then return ()
        else do
            h <- startSimulation pipe process simModel
            r1 <- sendRequest pipe (setGen1Act boostersNozzle1T)
            r2 <- sendRequest pipe (setGen2Act boostersNozzle2T)
            when (r1 /= ok) $ error "Failed to set value gen to boostersNozzle1T."
            when (r2 /= ok) $ error "Failed to set value gen to boostersNozzle2T."
            putMVar ref h
            print "Simulation started."
        
toggleSimulationOff pipe simModel ref = do
    simStopped <- isEmptyMVar ref
    if simStopped
        then return ()
        else do
            h <- takeMVar ref
            stopSimulation h
            print "Simulation stopped."
    
createSimulatorWorkspaceViewModel p m = do
    ref <- newEmptyMVar
    return [ defMethod' "vmToggleSimulation" (\_ checked -> 
        if checked
           then toggleSimulationOn p m ref
           else toggleSimulationOff p m ref) ]
    
createWorkspaceViewModel (SimulatorWorkspace view pipe simModel) = do
    let file = T.pack $ view ++ "View.qml"
    methods <- createSimulatorWorkspaceViewModel pipe simModel
    return (file, methods)
    
shellViewModelClass :: (T.Text, [Member ()]) -> IO (Class ())
shellViewModelClass (workspaceFile, methods) = newClass $
    (defPropertyRO' "vmWorkspace" (\_ -> return workspaceFile :: IO T.Text))
    : methods

createShellViewModel workspace = do
    vm <- createWorkspaceViewModel workspace
    viewModelType <- shellViewModelClass vm
    viewModel <- newObject viewModelType ()
    return viewModel
    
startUiApplication workspace = do
    let view = fileDocument "app/Views/ShellView.qml"
    viewModel <- createShellViewModel workspace

    runEngineLoop QML.defaultEngineConfig {
        initialDocument = view,
        contextObject = Just $ QML.anyObjRef viewModel
    }

makeSimulator = do
    simModel <- compileSimModel networkDef
    pipe <- createPipe :: IO SimulatorPipe
    return (pipe, simModel)
        
main :: IO ()
main = do
    print "Andromeda Control Software, version 0.1"
    print "Loading sample spaceship network..."
    (pipe, simModel) <- makeSimulator
    
    
    let workspace = SimulatorWorkspace "SimulatorWorkspace" pipe simModel
    startUiApplication workspace
