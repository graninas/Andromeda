{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Common where

import Prelude hiding (read)
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad.Free
import Control.Monad.Trans.State as S
import Control.Monad.State.Class (MonadState(..))
import Control.Lens

import Andromeda.Types.Physics
import Andromeda.Types.Hardware
import Andromeda.Types.Language.Hardware
import Andromeda.Types.Language.Scripting
import Andromeda.Types.Common.Value

newtype TestInterpreterSt = TestInterpreterSt
  { _debugPrintEnabled :: Bool
  }

type TestCPInterpreter = StateT TestInterpreterSt IO

makeLenses ''TestInterpreterSt

debugPrint_ :: (Show v, MonadState TestInterpreterSt m, MonadIO m) => v -> m ()
debugPrint_ v = do
  dp <- use debugPrintEnabled
  when dp $ liftIO $ print v

instance ControlProgramInterpreter TestCPInterpreter where
  onEvalScript (ControllerScriptWrapper scr)     = interpretControllerScript scr
  onEvalScript (InfrastructureScriptWrapper scr) = interpretInfrastructureScript scr

instance ControllerScriptInterpreter TestCPInterpreter where
  onGet c p     = debugPrint_ ("Get", c, p)      >> return (StringValue "ggg")
  onSet c p v   = debugPrint_ ("Set", c, p, v)
  onRead c ci p = debugPrint_ ("Read", c, ci, p) >> return (Measurement . FloatValue $ 33.3)
  onRun c cmd   = debugPrint_ ("Run", c, cmd)    >> return (Right "OK.")

instance InfrastructureScriptInterpreter TestCPInterpreter where
  onSendTo r v     = debugPrint_ ("SendTo", v)
  onGetCurrentTime = debugPrint_ "GetCurrentTime" >> return 10

testInterpretControllerScript debugPrint script
  = runStateT (interpretControllerScript script) (TestInterpreterSt debugPrint)

testInterpretInfrastructureScript debugPrint script
  = runStateT (interpretInfrastructureScript script) (TestInterpreterSt debugPrint)

testInterpretControlProgram debugPrint script
  = runStateT (interpretControlProgram script) (TestInterpreterSt debugPrint)

start = Command "start"
stop  = Command "stop"
power _ = Command "power"

logReceiver :: Receiver
logReceiver = print

alarmReceiver :: Receiver
alarmReceiver v = print ("WARNING!", v)

readTemperature :: Controller -> ComponentIndex -> ControllerScript (Measurement Kelvin)
readTemperature controller idx = read controller idx temperature

readPressure :: Controller -> ComponentIndex -> ControllerScript (Measurement Pascal)
readPressure controller idx = read controller idx pressure
