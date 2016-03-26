{-# LANGUAGE DeriveFunctor #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Language
import Control.Monad.Free

-- TODO: this is wrong. Action DSL shouldn't depend on receiver from environment.
data Receiver = Database | Reporter

data Action a
    = Ask Controller Property (Value -> a)
    | Read Controller Parameter (Data -> a)
    | Run Controller Command a
    | SendTo [Receiver] Data a -- Should it be async? Or we can use explicit annotation of it to be async?
  deriving (Functor)

type Script a = Free Action a

ask :: Controller -> Property -> Script Value
ask c p = liftF (Ask c p id)

read :: Controller -> Parameter -> Script Data
read c p = liftF (Read c p id)

run :: Controller -> Command -> Script Value
run c cmd = liftF (Run c cmd ())

sendTo :: [Receiver] -> Data -> Script ()
sendTo rs d = liftF (SendTo rs d ())

reporter = Reporter
database = Database


