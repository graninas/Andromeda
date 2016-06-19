{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module Andromeda.LogicControl.Language.Controller where

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
    | SendTo Receiver Value a -- TODO: move into another language
  deriving (Functor)

type ControllerScriptT tag a = Free (Procedure tag) a
type ControllerScriptM tag = ControllerScriptT tag (Measurement tag)
type ControllerScriptV a = ControllerScriptT () a
type ControllerScript a = forall tag. ControllerScriptT tag a

ask :: Controller -> Property -> ControllerScript Value
ask c p = liftF (Ask c p id)

read :: Controller -> Parameter tag -> ControllerScriptM tag
read c p = liftF (Read c p id)

readCelsius :: Controller -> Parameter Celsius -> ControllerScriptM Celsius
readCelsius c p = liftF (ReadCelsius c p id)

readKelvin :: Controller -> Parameter Kelvin -> ControllerScriptM Kelvin
readKelvin c p = liftF (ReadKelvin c p id)

run :: Controller -> Command -> ControllerScript ()
run c cmd = liftF (Run c cmd ())
-- Example without liftF:
--run c cmd = Free (Run c cmd (Pure ()))

sendTo :: Receiver -> Value -> ControllerScript ()
sendTo r v = liftF (SendTo r v ())

status = Status

