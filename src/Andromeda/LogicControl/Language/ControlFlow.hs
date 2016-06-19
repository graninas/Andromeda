{-# LANGUAGE Arrows #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Andromeda.LogicControl.Language.ControlFlow where

import Andromeda.LogicControl.Language.Controller
import Andromeda.LogicControl.Language.Control
import Andromeda.LogicControl.Language.Script

import Control.Arrow.ArrEff
import Control.Arrow
import Control.Monad.Free

type FlowArr b c = ArrEffFree Control b c

evalScriptA :: FlowArr (Script b) b
evalScriptA = mArr evalScript

evalComputationA :: FlowArr (Computation b) b
evalComputationA = mArr evalComputation
