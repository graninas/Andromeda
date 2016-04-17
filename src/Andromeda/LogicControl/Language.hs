{-# LANGUAGE DeriveFunctor #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Language
import Andromeda.Common.Value
import Control.Monad.Free
import Prelude hiding (read)

type Receiver = Value -> IO ()

data Procedure a
    = Ask Controller Property (Value -> a)
    | Read Controller Parameter (Value -> a)
    | Run Controller Command a
    | SendTo Receiver Value a
  deriving (Functor)

type Script a = Free Procedure a

ask :: Controller -> Property -> Script Value
ask c p = liftF (Ask c p id)

read :: Controller -> Parameter -> Script Value
read c p = liftF (Read c p id)

run :: Controller -> Command -> Script ()
run c cmd = liftF (Run c cmd ())

sendTo :: Receiver -> Value -> Script ()
sendTo r v = liftF (SendTo r v ())



