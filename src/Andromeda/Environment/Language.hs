module Andromeda.Environment.Language where

import Andromeda.ControlLogic.Language
import Andromeda.System.Language
import Control.Monad.Free

type Period = Float

data Times = 
    | Once
    | Times Int Period
    | Infinitely Period
  deriving (Show)

data ScriptAction a
    = Eval Times (Script a) a
  deriving (Functor)

type ControlScript a = Free ScriptAction a

data Procedure a
    = Load Scheme a
    | With HardwareHint (Controller -> ControlScript a) a
    | Wait Float a
    | Log String a
  deriving (Functor)


type ControlProgram a = Free Procedure a



