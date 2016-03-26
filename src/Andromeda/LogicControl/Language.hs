{-# LANGUAGE DeriveFunctor #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Language

import Control.Monad.Free

data Procedure a
    = Ask Controller Parameter (Value -> a)
    | Run Controller Command a
    | Send Parameter Value Receiver a -- Should it be async? Or we can use explicit annotation of it to be async?
  deriving (Functor)


type Script a = Free Procedure a


