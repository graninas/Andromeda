{-# LANGUAGE DeriveFunctor #-}

module Andromeda.HAL.Language where

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Free


data Hardware
    = Hardware { _name :: BS.ByteString
               , _manufacturer :: BS.ByteString }

type Index = (Int, Int)
type Address = BS.ByteString
data Profile
    = Profile
    | Scheme { _scheme :: V.Vector (V.Vector Hardware)
             , _mapping :: M.Map Address Index }

data Condition = Condition
data Parameter = Status | Temperature
data Command = Start | Stop | Power Float
data Controller = Controller

-- TODO
data Value = Value
           | BoolValue Bool
           | FloatValue Float
  deriving (Eq, Show)

data Procedure a
    = LoadProfile Profile a
    | GetHardware Address (Hardware -> a)
    | ValidateCondition Condition a
    | InitController Hardware (Controller -> a)
    | AskParameter Controller Parameter (Value -> a)
    | RunCommand Controller Command a
    | Report String String a
    | Save Parameter Value a
    | Wait Float a
  deriving (Functor)

type Script = Free Procedure

load :: ProfileHint -> Profile -> Script ()
load ph p = liftF (LoadProfile p ())

get :: HardwareHint -> Address -> Script Hardware
get hh a = liftF (GetHardware a id)

validate :: Condition -> Script ()
validate cond = liftF (ValidateCondition cond ())

init :: Hardware -> Script Controller
init h = liftF (InitController h id)

ask :: Controller -> Parameter -> Script Value
ask c p = liftF (AskParameter c p id)

command :: Controller -> Command -> Script ()
command c cmd = liftF (RunCommand c cmd ())

report :: String -> String -> Script ()
report s1 s2 = liftF (Report s1 s2 ())

wait :: Float -> Script ()
wait time = liftF (Wait time ())

save :: Parameter -> Value -> Script ()
save p v = liftF (Save p v ())

data ProfileHint = ProfileHint
data HardwareHint = HardwareHint

scheme = ProfileHint
hardware = HardwareHint

status = Status
temperature = Temperature
start = Start
stop = Stop
power = Power

float = FloatValue
int = BoolValue
