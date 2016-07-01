{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Andromeda.LogicControl.Language.Controller where

import Andromeda.Common
import Andromeda.Hardware
import Andromeda.Calculations

import Control.Monad.Free
import Prelude hiding (read)

-- Dummy types, should be designed later
data Property = Time | Status
  deriving (Show, Read, Eq)
data Command = Command String (Maybe Value)
  deriving (Show, Read, Eq)
newtype Controller = Controller String
  deriving (Show, Read, Eq)
type CommandResult = Either String String

-- TODO: rework ComponentIndex, ValueSource and relations with HDL.
data Procedure a
    = Get Controller Property (Value -> a)
    | Set Controller Property Value a
    | forall tag. Read Controller ComponentIndex (Parameter tag) (Measurement tag -> a)
    | Run Controller Command (CommandResult -> a)

instance Functor Procedure where
    fmap g (Get c p f)      = (Get c p     (g . f))
    fmap g (Set c p v next) = (Set c p v   (g next))
    fmap g (Read c ci p f)  = (Read c ci p (g . f))
    fmap g (Run c cmd f)    = (Run c cmd   (g . f))

type ControllerScript a = Free Procedure a

get :: Controller -> Property -> ControllerScript Value
get c p = liftF (Get c p id)

set :: Controller -> Property -> Value -> ControllerScript ()
set c p v = liftF (Set c p v ())

read :: forall tag. Controller -> ComponentIndex -> Parameter tag -> ControllerScript (Measurement tag)
read c idx p = liftF (Read c idx p id)

run :: Controller -> Command -> ControllerScript CommandResult
run c cmd = liftF (Run c cmd id)

status = Status

