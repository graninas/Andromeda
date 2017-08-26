{-# LANGUAGE Arrows #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Andromeda.Types.Language.ControlFlow.ControlFlow where

import Andromeda.Types.Language.Scripting.ControllerScript
import Andromeda.Types.Language.Scripting.ControlProgram
import Andromeda.Types.Language.Scripting.ScriptWrapper

import Control.Arrow.ArrEff
import Control.Arrow
import Control.Monad.Free

type FlowArr b c = ArrEffFree Control b c

evalScriptA :: FlowArr (Script b) b
evalScriptA = mArr evalScript
