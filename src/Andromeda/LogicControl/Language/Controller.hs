{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

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
data Procedure tag a
    = Get Controller Property (Value -> a)
    | Set Controller Property Value a
    | Read Controller ComponentIndex (Parameter tag) (Measurement tag -> a)
    | Run Controller Command (CommandResult -> a)
  deriving (Functor)

type ControllerScriptT tag a = Free (Procedure tag) a
type ControllerScriptM tag = ControllerScriptT tag (Measurement tag)
type ControllerScriptV a = ControllerScriptT () a
type ControllerScript a = forall tag. ControllerScriptT tag a

get :: Controller -> Property -> ControllerScript Value
get c p = liftF (Get c p id)

set :: Controller -> Property -> Value -> ControllerScript ()
set c p v = liftF (Set c p v ())

read :: Controller -> ComponentIndex -> Parameter tag -> ControllerScriptM tag
read c idx p = liftF (Read c idx p id)

run :: Controller -> Command -> ControllerScript CommandResult
run c cmd = liftF (Run c cmd id)

status = Status

