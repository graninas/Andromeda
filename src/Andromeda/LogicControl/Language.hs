{-# LANGUAGE DeriveFunctor #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Language
import Andromeda.Common.Value
import Control.Monad.Free
import Prelude hiding (read)

import Unsafe.Coerce

type Receiver = Value -> IO ()

data Procedure tag a
    = Ask Controller Property (Value -> a)
    | Read Controller (Parameter tag) (Measurment tag -> a)
    | Run Controller Command a
    | SendTo Receiver Value a
  deriving (Functor)

type Script tag a = Free (Procedure tag) a

ask :: Controller -> Property -> Script a Value
ask c p = liftF (Ask c p id)

read :: Controller -> Parameter tag -> Script tag (Measurment tag)
read c p = liftF (Read c p id)

run :: Controller -> Command -> Script a ()
run c cmd = liftF (Run c cmd ())

sendTo :: Receiver -> Value -> Script a ()
sendTo r v = liftF (SendTo r v ())

-- Don't know how to do this rihgt in Haskell...
unwrap p = unsafeCoerce p

