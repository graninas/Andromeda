{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Andromeda.Hardware.Handle where

import Andromeda.Hardware.Types
import Andromeda.Hardware.Parameter
import Andromeda.Calculations.Measurements

class ControllerLike a where
    getPhysicalAddress :: a -> PhysicalAddress

data ControllerLike contr => HardwareHandle contr = HardwareHandle {
    hardwareRead :: forall tag. contr -> ComponentIndex -> Parameter tag -> IO (Measurement tag)
}


