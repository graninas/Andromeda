import QtQuick 2.0
import QtQuick.Controls 1.0

Row {
    anchors.margins: 5
    anchors.fill: parent

    Column {
        spacing: 5
        width: 200
        height: parent.height

        Text {
            width: parent.width
            text: vmWorkspace.vmText
            color: "green"
            font.pointSize: 11
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