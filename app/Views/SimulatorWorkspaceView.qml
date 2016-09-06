import QtQuick 2.0
import QtQuick.Controls 1.0

Row {
    anchors.margins: 5
    anchors.fill: parent

    Column {
        spacing: 5
        width: 200
        height: parent.height

        Rectangle {
            border.width: 1
            border.color: "gold"
            height: parent.height
            width: parent.width

            ListView {
                id: devicesList
                anchors.fill: parent
                anchors.margins: 3
                currentIndex: 0
                clip: true
                focus: true
                
                Rectangle {
                    anchors.fill: parent
                    color: "lightgrey"
                    z: -1
                    radius: 2
                }
                
                model: vmWorkspace
                
                delegate: Text {
                    width: parent.width
                    text: vmText
                    color: "green"
                    font.pointSize: 11
                    z: 5
                    
                    MouseArea {
                        anchors.fill: parent
                        onClicked: { devicesList.currentIndex = index }
                    }
                }
            }
        }
    }
    
    Column {
        spacing: 5
        width: parent.width - 200
        height: parent.height
        
        Rectangle {
            anchors.margins: 3
            border.width: 1
            border.color: "brown"
            height: parent.height
            width: parent.width
        }
    }

}