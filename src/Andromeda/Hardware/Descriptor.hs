module Hardware.Descriptor where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS

data Descriptor
    = Descriptor { _hardwareGuid :: BS.ByteString
                 , _hardwareName :: BS.ByteString
                 , _hardwareManufacturer :: BS.ByteString }
