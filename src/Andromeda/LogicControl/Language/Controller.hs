{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Andromeda.LogicControl.Language.Controller where

import Andromeda.Common
import Andromeda.Hardware hiding (interpret)
import Andromeda.Calculations

import Control.Monad.Free
import Prelude hiding (read)
import qualified Data.ByteString.Char8 as BS

-- Dummy types, should be designed later
data Property = Time | Status
  deriving (Show, Read, Eq)
data Command = Command String
  deriving (Show, Read, Eq)
newtype Controller = Controller BS.ByteString
  deriving (Show, Read, Eq)
type CommandResult = Either String String

-- TODO: rework ComponentIndex, ValueSource and relations with HDL.
-- TODO: Parameter seems redundant because component index is related to the concrete sensor,
-- thus the information about measurement type is already there.
data Procedure a
    = Get Controller Property (Value -> a)
    | Set Controller Property Value a
    | forall tag. Read Controller ComponentIndex (Parameter tag) (Measurement tag -> a)
    | Run Controller Command (CommandResult -> a)

instance Functor Procedure where
    fmap g (Get c p f)      = (Get c p     (g . f))
    fmap g (Set c p v next) = (Set c p v   (g next))
    fmap g (Read c ci p f)  = (Read c ci p (g . f))
    fmap g (Run c cmd f)    = (Run c cmd   (g . f))
   
type ControllerScript a = Free Procedure a

class Monad m => ControllerScriptInterpreter m where
    onGet  :: Controller -> Property -> m Value
    onSet  :: Controller -> Property -> Value -> m ()
    onRead :: forall tag. Controller -> ComponentIndex -> Parameter tag -> m (Measurement tag)
    onRun  :: Controller -> Command -> m CommandResult
    
interpretControllerScript :: (Monad m, ControllerScriptInterpreter m) => ControllerScript a -> m a
interpretControllerScript (Pure a) = return a
interpretControllerScript (Free (Get c p next)) = do
    v <- onGet c p
    interpretControllerScript $ next v
interpretControllerScript (Free (Set c p v next)) = do
    onSet c p v
    interpretControllerScript next
interpretControllerScript (Free (Read c ci p next)) = do
    v <- onRead c ci p
    interpretControllerScript $ next v
interpretControllerScript (Free (Run c cmd next)) = do
    v <- onRun c cmd
    interpretControllerScript $ next v
    
get :: Controller -> Property -> ControllerScript Value
get c p = liftF (Get c p id)

set :: Controller -> Property -> Value -> ControllerScript ()
set c p v = liftF (Set c p v ())

read :: forall tag. Controller -> ComponentIndex -> Parameter tag -> ControllerScript (Measurement tag)
read c idx p = liftF (Read c idx p id)

run :: Controller -> Command -> ControllerScript CommandResult
run c cmd = liftF (Run c cmd id)

status = Status

