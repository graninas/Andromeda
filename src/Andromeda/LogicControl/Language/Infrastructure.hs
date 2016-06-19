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

type Receiver = Value -> IO ()

type DbValue = (ValueSource, Measurement Kelvin, (String, Float))

-- TODO: can be storing of value unified with SendTo?
data Action a = StoreValue DbValue a
              | SendTo Receiver Value a
  deriving (Functor)

type InfrastructureScript a = Free Action a

storeValue :: DbValue -> InfrastructureScript ()
storeValue val = liftF $ StoreValue val ()

sendTo :: Receiver -> Value -> InfrastructureScript ()
sendTo r v = liftF (SendTo r v ())