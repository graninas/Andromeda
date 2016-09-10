{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module ViewModels.DeviceComponentViewModel where

import Andromeda

import Graphics.QML as QML
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Typeable
import Data.Proxy
import Control.Concurrent.MVar
import Control.Monad (when)


data DeviceComponentVM = DeviceComponentVM
    { _deviceComponent :: DeviceComponent
    }
    
instance DefaultClass DeviceComponentVM where
  classMembers = [ defPropertyConst' "vmDeviceComponentView" view' ]
    where
        view' objRef = let componentVM = fromObjRef objRef
                       in case getComponentClass $ _deviceComponent componentVM of
                            Sensors -> return $ T.pack "SensorItemView.qml"
                            Controllers -> return $ T.pack "ControllerItemView.qml"

createDeviceComponentVM component = newObjectDC (DeviceComponentVM component)
    