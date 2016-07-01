{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module Andromeda.LogicControl.Language.Infrastructure where

import Andromeda.Common
import Andromeda.Hardware hiding (interpret)
import Andromeda.Calculations

import Control.Monad.Free
import Prelude hiding (read)

-- Raw dummy types and language instructions.
-- TODO: design it.
type Time = Int
type Receiver = Value -> IO ()

-- TODO: remove store reading from here to DataAccessScript
data Action a = --StoreReading Reading a
                SendTo Receiver Value a
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
    
--storeReading :: Reading -> InfrastructureScript ()
--storeReading reading = liftF $ StoreReading reading ()

sendTo :: Receiver -> Value -> InfrastructureScript ()
sendTo r v = liftF (SendTo r v ())

getCurrentTime :: InfrastructureScript Time
getCurrentTime = liftF (GetCurrentTime id)



