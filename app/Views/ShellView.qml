import QtQuick 2.0
import QtQuick.Controls 1.0

ApplicationWindow {
    id: andromedaWindow
    title: "Andromeda Software"
    width: 1024
    height: 768
    visible: true
    
    Rectangle {
        id: workspaceLoaderArea
        anchors.fill: parent
        border.width: 3
        border.color: "#CCCCCC"
        Loader { id: workspaceLoader }
    }
        
    Component.onCompleted: { workspaceLoader.source = vmWorkspaceView }
}