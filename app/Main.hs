module Main where

import Andromeda

import Graphics.QML as QML
import System.Directory
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

shellViewModelClass :: T.Text -> IO (Class ())
shellViewModelClass workspaceFile = newClass [
        defPropertyRO' "workspace" (\_ -> return workspaceFile :: IO T.Text)
    ]

createShellViewModel (Workspace w) = do
    let workspaceViewFile = T.pack $ w ++ "View.qml"
    viewModelType <- shellViewModelClass workspaceViewFile
    viewModel <- newObject viewModelType ()
    return viewModel
    
startUiApplication workspace = do
    let view = fileDocument "app/Views/ShellView.qml"
    viewModel <- createShellViewModel workspace

    runEngineLoop QML.defaultEngineConfig {
        initialDocument = view,
        contextObject = Just $ QML.anyObjRef viewModel
    }

data Workspace = Workspace String
        
main :: IO ()
main = do
    print "Andromeda Control Software, version 0.1"
    let workspace = Workspace "NetworkWorkspace"
    startUiApplication workspace
