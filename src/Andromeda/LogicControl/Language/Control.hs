{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Andromeda.LogicControl.Language.Control where

import Andromeda.LogicControl.Language.Script

import Control.Monad.Trans.Free
import qualified Control.Monad.Free as F

data Control a = forall b. EvalScript (Script b) (b -> a)
               

instance Functor Control where
    fmap f (EvalScript scr g) = EvalScript scr (f . g)

type ControlProgram a = F.Free Control a

class Monad m => ControlProgramInterpreter m where
    onEvalScript :: Script a -> m a
    
interpretControlProgram :: (Monad m, ControlProgramInterpreter m) => ControlProgram a -> m a
interpretControlProgram (F.Pure a) = return a
interpretControlProgram (F.Free (EvalScript scr next)) = do
    r <- onEvalScript scr
    interpretControlProgram $ next r
    
    
evalScript :: Script a -> ControlProgram a
evalScript scr = F.liftF (EvalScript scr id)
