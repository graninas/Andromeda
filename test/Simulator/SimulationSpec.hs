{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module Simulator.SimulationSpec where

import Lib
import Andromeda
import TestCommon

import Test.Hspec
import Prelude hiding (read)
import Control.Monad
import Control.Monad.Trans.State as S
import Control.Monad.State.Class (MonadState)
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Concurrent
import Control.Lens
import Data.List (nub, sort)

data In = Initialize
        | SimStateSimpleAction (SimState ())
        | GetValueSource ComponentInstanceIndex
        | Start ComponentInstanceIndex
        | Stop ComponentInstanceIndex
data Out = Out String
         | OutValueSource ValueSource

instance Eq Out where
    Out s1 == Out s2 = s1 == s2
    OutValueSource v1 == OutValueSource v2 = False
    _ == _ = False
  
instance Show Out where
    show (Out s1) = "Out " ++ s1
    show (OutValueSource v1) = "OutValueSource"
  
type SimulatorPipe = Pipe In Out

ok = Out "OK."

process :: Process In Out
process Initialize = return ok
process (SimStateSimpleAction act) = act >> return ok
process (GetValueSource idx) = do
    v <- getValueSource idx
    return $ OutValueSource v

makeRunningSimulation = do
    simModel <- compileSimModel networkDef
    pipe <- createPipe :: IO SimulatorPipe
    simHandle <- startSimulation pipe process simModel
    return (pipe, simHandle)

simulateSingleReq req = do
    (pipe, simHandle) <- makeRunningSimulation
    resp <- sendRequest pipe req
    stopSimulation simHandle
    return resp
    
okOnSuccessAction act = SimStateSimpleAction act

runNetworkAct = okOnSuccessAction $ runNetwork
setGen1Act idx = okOnSuccessAction $ setValueGenerator idx floatIncrementGen

newtype SimNetworkBridge a = SimNetworkBridge (StateT InterpreterSt IO a)
    deriving (Functor, Applicative, Monad, MonadState InterpreterSt, MonadIO)

instance ControlProgramInterpreter SimNetworkBridge where
    onEvalScript (ControllerScript scr)     = interpretControllerScript scr
    onEvalScript (InfrastructureScript scr) = interpretInfrastructureScript scr

instance ControllerScriptInterpreter SimNetworkBridge where
    onGet contr prop = do
        debugPrint_ ("Get", contr, prop)
        error "onGet not implemented."
    onSet contr prop v = do
        debugPrint_ ("Set", contr, prop, v)
        error "onSet not implemented."
    onRead contr compIdx param = do
        debugPrint_ ("Read", contr, compIdx, param)
        error "onRead not implemented."
    onRun contr cmd = do
        debugPrint_ ("Run", contr, cmd)
        error "onRun not implemented."
    
instance InfrastructureScriptInterpreter SimNetworkBridge where
    onSendTo r v = do
        debugPrint_ ("SendTo", v)
        error "onSendTo not implemented."
    onGetCurrentTime = do
        debugPrint_ "GetCurrentTime"
        error "onGetCurrentTime not implemented."

readTemperatureSensor :: ComponentInstanceIndex -> ControlProgram (Measurement Kelvin)
readTemperatureSensor (devObjIdx, compIdx) = evalScript . controllerScript $ do
    read (Controller "TODO") compIdx temperature

readSensorTimes :: Int -> ComponentInstanceIndex -> ControlProgram [Measurement Kelvin]
readSensorTimes n = sequence . replicate n . readTemperatureSensor

{- ComponentIndex :: ByteString
   DeviceObjectIndex = ByteString

sensors in one device:
  (ComponentIndex, sensorobj)

sensors composing:
  (ComponentIndex, sensorobj)
  
sensors model (all sensors in all devices):

  ComponentInstanceIndex :: (DeviceObjectIndex, ComponentIndex)
  (For now it's the same as DeviceIndex. It's simulation-only type.)

    (ComponentInstanceIndex, sensorobj)
  ~ ((DeviceObjectIndex, ComponentIndex), sensorobj)
-}

spec = describe "Simulation test" $ do
    it "Initialization should be successfull." $
        simulateSingleReq Initialize `shouldReturn` ok
    it "Setting of value generator to unattached pipe should throw." $ do
        pipe <- createPipe :: IO SimulatorPipe
        sendRequest pipe (setGen1Act boostersNozzle1T) `shouldThrow` anyException
    it "Setting of value generator should be successfull." $
        simulateSingleReq (setGen1Act boostersNozzle1T) `shouldReturn` ok
    it "Continuous simulation of sensor should return values." $ do
        (pipe, simHandle) <- makeRunningSimulation
        r1 <- sendRequest pipe (setGen1Act boostersNozzle1T)
        r2 <- sendRequest pipe runNetworkAct
        (OutValueSource vs) <- sendRequest pipe (GetValueSource boostersNozzle1T)
        vals <- sequence (replicate 10 $ threadDelay 1000 >> readValueSource vs) -- TODO: this is unstable.
        stopSimulation simHandle
        r1 `shouldBe` ok
        r2 `shouldBe` ok
        sort vals `shouldBe` vals
        length vals `shouldBe` 10
    