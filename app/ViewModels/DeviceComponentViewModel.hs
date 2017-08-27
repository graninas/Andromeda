{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module ViewModels.DeviceComponentViewModel where

import Graphics.QML as QML
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Typeable
import Data.Proxy
import Control.Concurrent.MVar
import Control.Monad (when)

import Andromeda.Simulator
import Andromeda.Types.Hardware

data DeviceComponentVM = DeviceComponentVM
  { _deviceComponent :: DeviceComponent
  }

instance DefaultClass DeviceComponentVM where
  classMembers = [ defPropertyConst' "vmDeviceComponentView" view' ]
    where
      view' objRef = case getComponentClass $ _deviceComponent (fromObjRef objRef) of
                        Sensors -> return $ T.pack "SensorItemView.qml"
                        Controllers -> return $ T.pack "ControllerItemView.qml"

createDeviceComponentVM component = newObjectDC (DeviceComponentVM component)
