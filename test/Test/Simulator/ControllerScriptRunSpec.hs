{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Simulator.ControllerScriptRunSpec where

import Prelude hiding (read)
import Control.Monad
import Control.Monad.Trans.State as S
import Control.Monad.State.Class (MonadState)
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Concurrent
import Control.Lens
import Data.List (nub, sort)
import Test.Hspec

import Lib
import Test.Common

import Andromeda.Types.Hardware
import Andromeda.Types.Physics
import Andromeda.Types.Language.Scripting

import Andromeda.Assets.SpaceshipSample

newtype InterpreterSt = InterpreterSt
  { _hardwareHandle :: HardwareHandle
  }

newtype SimNetworkBridge a = SimNetworkBridge
  { runSimNetworkBridge :: StateT InterpreterSt IO a
  }
  deriving (Functor, Applicative, Monad, MonadState InterpreterSt, MonadIO)

makeLenses ''InterpreterSt

debugPrint :: (Show v, MonadIO m) => v -> m ()
debugPrint = liftIO . print

instance ControlProgramInterpreter SimNetworkBridge where
  onEvalScript (ControllerScriptWrapper scr) = interpretControllerScript scr
  onEvalScript _ = error "Not implemented."

instance ControllerScriptInterpreter SimNetworkBridge where
  onGet c p     = debugPrint ("Get", c, p) >> error "Not implemented."
  onSet c p v   = debugPrint ("Set", c, p, v) >> error "Not implemented."
  onRun c cmd   = debugPrint ("Run", c, cmd) >> error "Not implemented."
  onRead c ci p = do
    h <- use hardwareHandle
    liftIO $ hardwareRead h c ci p

readTemperatureSensor :: ComponentInstanceIndex -> ControlProgram (Measurement Kelvin)
readTemperatureSensor (addr, compIdx) = evalScript . controllerScriptWrapper $
  read (Controller addr) compIdx temperature

readSensorTimes :: Int -> ComponentInstanceIndex -> ControlProgram [Measurement Kelvin]
readSensorTimes n = replicateM n . readTemperatureSensor

runControlProgram :: ControlProgram [Measurement Kelvin] -> HardwareHandle -> IO [Measurement Kelvin]
runControlProgram prog handle = result
  where
    ev :: SimNetworkBridge  [Measurement Kelvin]
    ev = interpretControlProgram prog
    result = S.evalStateT (runSimNetworkBridge ev) (InterpreterSt handle)

spec = describe "Simulation test" $
  it "Controller script evaluation should return values."
    pending
    -- (pipe, simHandle) <- makeRunningSimulation
    -- r1 <- sendRequest pipe (setGen1Act boostersNozzle1T)
    -- r3 <- sendRequest pipe runNetworkAct
    --
    -- let program1 = readSensorTimes 1 boostersNozzle1T
    -- let program2 = readSensorTimes 2 boostersNozzle1T
    --
    -- (OutHardwareHandle hHandle) <- sendRequest pipe GetHardwareHandle
    -- [val1] <- runControlProgram program1 hHandle
    -- [val2, val3] <- runControlProgram program2 hHandle
    --
    -- stopSimulation simHandle
    --
    -- val1 >= toKelvin 0.0 `shouldBe` True
    -- val2 >= val1 `shouldBe` True
    -- val3 >= val2 `shouldBe` True
