import QtQuick 2.0
import QtQuick.Controls 1.0

ApplicationWindow {
    id: andromedaWindow
    title: "Andromeda Software"
    width: 1024
    height: 768

    visible: true
    
    Loader { 
        id: workspaceLoader
//        width: andromedaWindow.width
//        height: andromedaWindow.height
    }
        
    Component.onCompleted: {
        workspaceLoader.source = vmWorkspaceFile
    }
}