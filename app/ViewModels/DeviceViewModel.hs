{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module ViewModels.DeviceViewModel where

import Graphics.QML as QML
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Typeable
import Data.Proxy
import Control.Concurrent.MVar
import Control.Monad (when)

import Andromeda.Simulator
import Andromeda.Types.Hardware
import ViewModels.DeviceComponentViewModel


data DeviceVM = DeviceVM
  { _device :: Device
  , _deviceComponents :: MVar [ObjRef DeviceComponentVM]
  }

data ComponentsChanged deriving Typeable

instance SignalKeyClass ComponentsChanged where
  type SignalParams ComponentsChanged = IO ()

instance DefaultClass DeviceVM where
  classMembers = [ defPropertyConst' "vmDeviceName" (return . T.pack . getDeviceId . _device . fromObjRef)
                 , defPropertySigRO "vmComponents" (Proxy :: Proxy ComponentsChanged) getComponents'
                 ]
    where
        getComponents' = readMVar . _deviceComponents . fromObjRef

createDeviceVM device = do
  componentVMs <- mapM createDeviceComponentVM (getComponents device)
  componentsMVar <- newMVar componentVMs
  newObjectDC $ DeviceVM device componentsMVar
