{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module Andromeda.LogicControl.Language.Infrastructure where

import Andromeda.Common
import Andromeda.Hardware
import Andromeda.Calculations

import Control.Monad.Free
import Prelude hiding (read)

-- Raw dummy types.
-- TODO: design it.

type DbValue = (ValueSource, Measurement Kelvin, (String, Float))

data Action a = StoreValue DbValue a
  deriving (Functor)

type InfrastructureScript a = Free Action a

storeValue :: DbValue -> InfrastructureScript ()
storeValue val = liftF $ StoreValue val ()