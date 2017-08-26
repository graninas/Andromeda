{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Andromeda.Types.Hardware.Handle where

import Andromeda.Types.Hardware.Types
import Andromeda.Types.Hardware.Parameter
import Andromeda.Types.Physics.Measurement

data HardwareHandle = HardwareHandle
  { hardwareRead :: forall tag c. ControllerLike c => c -> ComponentIndex -> Parameter tag -> IO (Measurement tag)
  }
