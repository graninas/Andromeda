{-# LANGUAGE OverloadedStrings #-}

module Andromeda.LogicControl.Interpreter where

-- Generalized interpreter which uses IoC container to get actions needed.


{- TODO: learn more on Haskell Type Families, GADTs and type level programming
    to make this possible:
    
LANGUAGE RankNTypes
LANGUAGE ImpredicativeTypes

import qualified Control.Monad.Trans.Reader as R
import Control.Monad (liftM)

type IoC a = Procedure () -> IO a
type Interpreter a b = R.ReaderT (IoC a) IO b

cunit = const ()
interpret :: Script a -> R.ReaderT (IoC a) IO b
interpret (Pure a) = return a
interpret (Free script) = case script of
    (InitController h fScript) -> do
        ioc <- R.ask
        res <- liftIO (ioc $ InitController h cunit)
        interpret (fScript res)
    (GetHardware a fScript) -> do
        ioc <- R.ask
        res <- liftIO (ioc $ GetHardware a cunit)
        interpret (fScript res)
    (LoadProfile p script) -> do
        ioc <- R.ask
        _ <- liftIO (ioc $ LoadProfile p ())
        interpret script
  ...
Error:
Couldn't match expected type ‘Hardware’ with actual type ‘a’
      ‘a’ is a rigid type variable bound by
          the type signature for
            interpret :: Script a -> R.ReaderT (IoC a) IO b
          at src/Andromeda/HAL/Interpreter.hs:20:14
    Relevant bindings include
      res :: a (bound at src/Andromeda/HAL/Interpreter.hs:29:9)
      ioc :: IoC a (bound at src/Andromeda/HAL/Interpreter.hs:28:9)
      fScript :: Hardware -> Free Procedure a
        (bound at src/Andromeda/HAL/Interpreter.hs:27:20)
      script :: Procedure (Free Procedure a)
        (bound at src/Andromeda/HAL/Interpreter.hs:22:17)
      interpret :: Script a -> R.ReaderT (IoC a) IO b
        (bound at src/Andromeda/HAL/Interpreter.hs:21:1)
    In the first argument of ‘fScript’, namely ‘res’
    In the first argument of ‘interpret’, namely ‘(fScript res)’
-}





