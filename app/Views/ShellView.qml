import QtQuick 2.0
import QtQuick.Controls 1.0

ApplicationWindow {
    id: andromedaWindow
    title: "Andromeda Software"
    width: 1024
    height: 768

    visible: true
    toolBar: ToolBar {
        id: toolbar
        Row {
            ToolButton {
                id: simulatorPlayButton
                visible: true
                checkable: true
                iconSource: "Images/Simulator.png"
                onClicked: vmToggleSimulation(simulatorPlayButton.checked)
            }
        }
    }
    
    Loader { 
        id: workspaceLoader
        width: andromedaWindow.width
        height: andromedaWindow.height - toolbar.height
    }
        
    Component.onCompleted: {
        workspaceLoader.source = vmWorkspaceFile
    }
}