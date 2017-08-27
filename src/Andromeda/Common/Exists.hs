{-# LANGUAGE RankNTypes, GADTs #-}
module Andromeda.Common.Exists where

import Unsafe.Coerce (unsafeCoerce)

data Exists f where
  Exists :: f a -> Exists f

mkExists :: forall f a. f a -> Exists f
mkExists = unsafeCoerce


runExists :: forall f r. (forall a. f a -> r) -> (Exists f -> r)
runExists = unsafeCoerce
