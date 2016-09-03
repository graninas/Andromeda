import QtQuick 2.0
import QtQuick.Controls 1.0


Rectangle {
    id: shellViewRect
    width: 1024
    height: 768
    
    Loader { id: workspaceLoader }
    
    Component.onCompleted: {
        workspaceLoader.source = workspace
    }
    
}