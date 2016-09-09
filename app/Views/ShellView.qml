import QtQuick 2.0
import QtQuick.Controls 1.0

ApplicationWindow {
    id: andromedaWindow
    title: "Andromeda Software"
    width: 1024
    height: 768
    visible: true
    
    Rectangle {
        anchors.fill: parent
        anchors.margins: 0
        border.width: 3
        border.color: "#929292"
        Loader { id: workspaceLoader }
    }
        
    Component.onCompleted: { workspaceLoader.source = vmWorkspaceFile }
}