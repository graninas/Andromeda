module Main where

import Andromeda

import Graphics.QML as QML

createViewModel = do
    viewModelType <- newClass [] :: IO (Class ())
    viewModel <- newObject viewModelType ()
    return viewModel

startUiApplication = do
    let view = fileDocument "app/Views/ShellView.qml"
    viewModel <- createViewModel

    runEngineLoop QML.defaultEngineConfig {
        initialDocument = view,
        contextObject = Just $ QML.anyObjRef viewModel
    }

        
main :: IO ()
main = do
    print "Andromeda Control Software, version 0.1"
    startUiApplication
