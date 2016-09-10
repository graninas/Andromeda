{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module ViewModels.ShellViewModel where

import Andromeda
import ViewModels.WorkspaceViewModel

import Graphics.QML as QML
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Typeable
import Data.Proxy
import Control.Concurrent.MVar
import Control.Monad (when)

data ShellVM = ShellVM
    { _shellWorkspaceVM :: ObjRef WorkspaceVM
    , _shellWorkspaceView :: T.Text
    } deriving (Typeable)

instance DefaultClass ShellVM where
    classMembers =
        [ defPropertyConst' "vmWorkspace" (return . _shellWorkspaceVM . fromObjRef)
        , defPropertyConst' "vmWorkspaceView" (return . _shellWorkspaceView . fromObjRef)
        ]
    
createShellVM (workspaceView, workspaceModel) = do
    workspaceVM <- newObjectDC workspaceModel
    let shellVM = ShellVM workspaceVM workspaceView
    newObjectDC shellVM