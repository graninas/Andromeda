{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Andromeda.Hardware.Hardware where

import Andromeda.Hardware.Description
import Andromeda.Hardware.HDL
import Andromeda.Hardware.Parameter
import Andromeda.Calculations
import Andromeda.Common

import Data.Typeable
import Data.Data
import Control.Monad.Free
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS


newtype Hardware = Hardware (M.Map HardwareName Par)
    deriving (Show, Eq)

blankHardware = Hardware M.empty


readParameter :: HardwareName -> Hardware -> Maybe Value
readParameter hName (Hardware ps) = case M.lookup hName ps of
    Nothing -> Nothing
    Just (Par p v) -> Just v
