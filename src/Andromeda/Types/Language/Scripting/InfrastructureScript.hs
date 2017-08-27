{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module Andromeda.Types.Language.Scripting.InfrastructureScript where

import Control.Monad.Free
import Prelude hiding (read)

import Andromeda.Types.Common.Value
import Andromeda.Types.Hardware hiding (interpret)

-- Raw dummy types and language instructions.
-- TODO: design it.
type Time = Int
type Receiver = Value -> IO ()

data Action a
  = SendTo Receiver Value a
  | GetCurrentTime (Time -> a)
  deriving (Functor)

type InfrastructureScript a = Free Action a

class Monad m => InfrastructureScriptInterpreter m where
  onSendTo :: Receiver -> Value -> m ()
  onGetCurrentTime :: m Time

interpretInfrastructureScript :: (Monad m, InfrastructureScriptInterpreter m) => InfrastructureScript a -> m a
interpretInfrastructureScript (Pure a) = return a
interpretInfrastructureScript (Free (SendTo r v next)) = do
  onSendTo r v
  interpretInfrastructureScript next
interpretInfrastructureScript (Free (GetCurrentTime next)) = do
  t <- onGetCurrentTime
  interpretInfrastructureScript $ next t

sendTo :: Receiver -> Value -> InfrastructureScript ()
sendTo r v = liftF (SendTo r v ())

getCurrentTime :: InfrastructureScript Time
getCurrentTime = liftF (GetCurrentTime id)
