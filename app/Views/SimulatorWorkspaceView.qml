import QtQuick 2.0
import QtQuick.Controls 1.0

Rectangle {
    height: parent.height
    width: parent.width
    anchors.fill: parent
    anchors.margins: 5
    Row {
        spacing: 5

        Column {
            spacing: 5
            width: 300
            anchors.top: parent.top
            anchors.bottom: parent.bottom

            Rectangle {
                id: simulatorToolbox
                height: 38
                anchors.left: parent.left
                anchors.right: parent.right
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
            
            Component {
                id: deviceItemComponent
                Item {
                    id: deviceItem
                    anchors.left: parent.left
                    anchors.right: parent.right
                    anchors.leftMargin: 10
                    anchors.rightMargin: 10
                    height: 30
                    
                    Text {
                        width: parent.width
                        font.pointSize: 12
                        text: modelData.vmDeviceName
                        color: deviceItem.ListView.isCurrentItem ? "red" : "black"
                    }
                    
                    MouseArea {
                        z: 1
                        hoverEnabled: false
                        anchors.fill: parent
                        onClicked: {
                            deviceItem.ListView.view.currentIndex = index
                            deviceItem.forceActiveFocus()                            
                        }
                    }
                }
            }
            ListView {
                id: devicesList
                anchors.left: parent.left
                anchors.right: parent.right
                height: 400
                spacing: 3
                clip: true
                focus: true
                model: vmWorkspace.vmDevices
                delegate: deviceItemComponent
                highlight: Rectangle { color: "lightsteelblue"; radius: 5 }
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