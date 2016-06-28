{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module Andromeda.LogicControl.Language.Infrastructure where

import Andromeda.Common
import Andromeda.Hardware
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

--storeReading :: Reading -> InfrastructureScript ()
--storeReading reading = liftF $ StoreReading reading ()

sendTo :: Receiver -> Value -> InfrastructureScript ()
sendTo r v = liftF (SendTo r v ())

getCurrentTime :: InfrastructureScript Time
getCurrentTime = liftF (GetCurrentTime id)


