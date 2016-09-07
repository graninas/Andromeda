import QtQuick 2.0
import QtQuick.Controls 1.0

Rectangle {
    anchors.fill: parent
    anchors.margins: 5
    Row {
        spacing: 5

        Column {
            spacing: 5
            width: 200

            Rectangle {
                id: simulatorToolbox
                height: 38
                anchors.margins: 5
                width: parent.width
                border.width: 1
                border.color: "blue"
                gradient: Gradient {
                    GradientStop { position: 0.0; color: "white" }
                    GradientStop { position: 1.0; color: "lightblue" }
                }
                
                Button {
                    id: simulatorToggleButton
                    anchors.left: parent.left
                    anchors.leftMargin: 3
                    anchors.verticalCenter: simulatorToolbox.verticalCenter
                    width: 32
                    height: 32
                    visible: true
                    checkable: true
                    iconSource: "Images/Simulator.png"
                    onClicked: vmWorkspace.vmToggleSimulation(simulatorToggleButton.checked)
                }
            }
            
            ListView {
                model: vmWorkspace.vmDevices
                delegate: Text {
                    font.pointSize: 12
                    text: vmText
                    color: "black"
                }
            }
        }
        
        Column {
            spacing: 5
            
            Rectangle {
                anchors.margins: 3
                border.width: 1
                border.color: "brown"
                height: parent.height
                width: parent.width
            }
        }
    }
}