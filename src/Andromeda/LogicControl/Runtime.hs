module Andromeda.Model.Runtime where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.Reader

type Runtime env a = ReaderT env STM a

