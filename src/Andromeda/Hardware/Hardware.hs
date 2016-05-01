module Andromeda.Hardware.Hardware (
    Hardware,
    blankHardware,
    addDevice,
    readParameter,
    makeHardware
  ) where

import Andromeda.Hardware.Description
import Andromeda.Hardware.HDL
import Andromeda.Hardware.Parameter
import Andromeda.Calculations
import Andromeda.Common

import Control.Monad.Free
import Control.Monad.State
import Unsafe.Coerce
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS

-- Pure instance of hardware.
-- Abstract data type.
newtype Hardware = HardwareImpl (M.Map HardwareName Par)
    deriving (Show, Eq)

blankHardware = HardwareImpl M.empty

readParameter :: HardwareName -> Hardware -> Maybe (Measurement tag)
readParameter hName (HardwareImpl ps) = case M.lookup hName ps of
    Nothing -> Nothing
     -- TODO: remove hack unsafeCoerce.
    Just (Par v m a) -> Just (unsafeCoerce $ convertAdmissible a undefined v)

addDevice :: BS.ByteString -> Par -> Hardware -> Hardware
addDevice hName p (HardwareImpl m) = HardwareImpl $ M.insert hName p m

interpret :: Hdl () -> State Hardware ()
interpret (Pure a)    = return a
interpret (Free proc) = case proc of
    Sensor _ hName p next -> do
        modify (addDevice hName p)
        interpret next

-- | Makes a real instanse of hardware defined by the language.
-- Operates in the State monad.
makeHardware :: Hdl () -> Hardware
makeHardware hdl = execState (interpret hdl) blankHardware

