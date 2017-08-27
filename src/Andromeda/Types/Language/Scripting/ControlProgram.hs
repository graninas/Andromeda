{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Andromeda.Types.Language.Scripting.ControlProgram where

import Andromeda.Types.Language.Scripting.ScriptWrapper

import Control.Monad.Trans.Free
import qualified Control.Monad.Free as F

-- TODO: ScriptWrapper can be replaced by existential / rankN type.
data Control a = forall b. EvalScript (ScriptWrapper b) (b -> a)


instance Functor Control where
    fmap f (EvalScript scr g) = EvalScript scr (f . g)

type ControlProgram a = F.Free Control a

class Monad m => ControlProgramInterpreter m where
  onEvalScript :: ScriptWrapper a -> m a

interpretControlProgram :: (Monad m, ControlProgramInterpreter m) => ControlProgram a -> m a
interpretControlProgram (F.Pure a) = return a
interpretControlProgram (F.Free (EvalScript scr next)) = do
  r <- onEvalScript scr
  interpretControlProgram $ next r

evalScript :: ScriptWrapper a -> ControlProgram a
evalScript scr = F.liftF (EvalScript scr id)
