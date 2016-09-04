import QtQuick 2.0
import QtQuick.Controls 1.0

ApplicationWindow {
    visible: true
    toolBar: ToolBar {
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
    
    Rectangle {
        id: shellViewRect
        width: 1024
        height: 768
        
        Loader { id: workspaceLoader }
        
        Component.onCompleted: {
            workspaceLoader.source = vmWorkspace
        }
        
    }
}