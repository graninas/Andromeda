{-# LANGUAGE DeriveFunctor #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Language
import Andromeda.Common.Value
import Control.Monad.Free
import Prelude hiding (read)

type Receiver = Value -> IO ()

data Procedure tag a
    = Ask Controller Property (Value -> a)
    | Read Controller (Parameter tag) (Measurment tag -> a)
    | Run Controller Command a
    | SendTo Receiver Value a
  deriving (Functor)

type Script tag a = Free (Procedure tag) a

data None

ask :: Controller -> Property -> Script None Value
ask c p = liftF (Ask c p id)

read :: Controller -> Parameter tag -> Script tag (Measurment tag)
read c p = liftF (Read c p id)

run :: Controller -> Command -> Script None ()
run c cmd = liftF (Run c cmd ())

sendTo :: Receiver -> Value -> Script None ()
sendTo r v = liftF (SendTo r v ())



