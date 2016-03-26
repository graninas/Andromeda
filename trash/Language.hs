{-Obsolete-}

{-# LANGUAGE DeriveFunctor #-}

module Andromeda.ControlEnvironment.Language where

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Free

import Hardware.Scheme

data Condition  = Condition
data Controller = Controller

data Action a
    = LoadScheme Scheme a
    | Wait Float a
    | Periodic Float a
    | InitController Hardware (Controller -> a)
    | DeinitController Controller a
    | Save Parameter Value a
  deriving (Functor)

type ControlProgram = Free Action

data SchemeHint = SchemeHint
data HardwareHint = HardwareHint

scheme = ProfileHint
hardware = HardwareHint

load :: SchemeHint -> Scheme -> ControlProgram ()
load _ s = liftF (LoadScheme s ())

get :: HardwareHint -> Address -> ControlProgram Descriptor
get _ addr = liftF (GetHardware addr id)

init :: Descriptor -> ControlProgram Controller
init h = liftF (InitController h id)

deinit :: Controller -> ControlProgram ()
deinit controller = liftF (DeinitController controller)

wait :: Float -> ControlProgram ()
wait time = liftF (Wait time ())

save :: Parameter -> Value -> ControlProgram ()
save p v = liftF (Save p v ())


status = Status
temperature = Temperature
start = Start
stop = Stop
power = Power

float = FloatValue
int = BoolValue
