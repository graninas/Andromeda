module Andromeda.Hardware.Network where

import Andromeda.Common
import Andromeda.Calculations

import Andromeda.Hardware.Parameter
import Andromeda.Hardware.Description

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS



type Index = (Int, Int)

type Address = BS.ByteString
data Scheme = Scheme { _scheme :: V.Vector Int -- TODO
                     , _mapping :: M.Map Address Index }

