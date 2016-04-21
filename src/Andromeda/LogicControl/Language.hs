{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware
import Andromeda.Common
import Andromeda.Calculations

import Control.Monad.Free
import Prelude hiding (read)

type Receiver = Value -> IO ()

newtype Controller = Controller String
  deriving (Show, Read, Eq)

data Command = Command String (Maybe Value)
  deriving (Show, Read, Eq)

data Property = Time | Status
  deriving (Show, Read, Eq)

data Procedure tag a
    = Ask Controller Property (Value -> a)
    | Read Controller (Parameter tag) (Measurement tag -> a)
    -- Special versions of Read to show it works with all tags.
    | ReadCelsius Controller (Parameter Celsius) (Measurement Celsius -> a)
    | ReadKelvin Controller  (Parameter Kelvin)  (Measurement Kelvin -> a)
    | Run Controller Command a
    | SendTo Receiver Value a
  deriving (Functor)

type ScriptT tag a = Free (Procedure tag) a
type ScriptM tag = ScriptT tag (Measurement tag)
type ScriptV a = ScriptT () a
type Script a = forall tag. ScriptT tag a

ask :: Controller -> Property -> Script Value
ask c p = liftF (Ask c p id)

read :: Controller -> Parameter tag -> ScriptM tag
read c p = liftF (Read c p id)

readCelsius :: Controller -> Parameter Celsius -> ScriptM Celsius
readCelsius c p = liftF (ReadCelsius c p id)

readKelvin :: Controller -> Parameter Kelvin -> ScriptM Kelvin
readKelvin c p = liftF (ReadKelvin c p id)

run :: Controller -> Command -> Script ()
run c cmd = liftF (Run c cmd ())

sendTo :: Receiver -> Value -> Script ()
sendTo r v = liftF (SendTo r v ())

status = Status

