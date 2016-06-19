{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Andromeda.LogicControl.Language.Control where

import Andromeda.LogicControl.Language.Controller
import Andromeda.LogicControl.Language.Script

import Control.Monad.Free

data Computation b = Computation

data Control a = forall b. EvalScript (Script b) (b -> a)
               | forall b. EvalComputation (Computation b) (b -> a)

instance Functor Control where
    fmap f (EvalScript scr g) = EvalScript scr (f . g)
    fmap f (EvalComputation cmp g) = EvalComputation cmp (f . g)

type ControlProgram a = Free Control a 

evalScript :: Script a -> ControlProgram a
evalScript scr = liftF (EvalScript scr id)

evalComputation :: Computation a -> ControlProgram a
evalComputation cmp = liftF (EvalComputation cmp id)


