module Hardware.Scheme where

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS

import Hardware.Descriptor

type Index = (Int, Int)

type Address = BS.ByteString
data Scheme = Scheme { _scheme :: V.Vector (V.Vector Descriptor)
                     , _mapping :: M.Map Address Index }
