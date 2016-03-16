module Andromeda.Model.HAL.Interpreter where

import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class

import Andromeda.HAL.Language
import Andromeda.Model.Runtime


interpret :: Script a -> Runtime env a
interpret (Pure a) = return a
interpret (Free script) = undefined -- TODO
