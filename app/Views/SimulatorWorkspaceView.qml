import QtQuick 2.0
import QtQuick.Controls 1.0
import QtQuick.Layouts 1.0

RowLayout {
    spacing: 5
    anchors.fill: parent
    anchors.margins: 5

    Rectangle {
        Layout.fillWidth: true
        Layout.minimumWidth: 300
        Layout.maximumWidth: 300
        Layout.preferredWidth: 300
        Layout.preferredHeight: workspaceLoaderArea.height-10
    
        ColumnLayout {
            anchors.fill: parent
            spacing: 5

            Rectangle {
                id: simulatorToolbox
                Layout.fillWidth: true
                Layout.alignment: Qt.AlignTop
                height: 38
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
            
            Rectangle {
                id: devicesArea
                Layout.fillWidth: true
                Layout.fillHeight: true
                border.width: 1
                border.color: "#CBE2F0"
                gradient: Gradient {
                    GradientStop { position: 0.0; color: "#CBF0EE" }
                    GradientStop { position: 1.0; color: "#CBE2F0" }
                }
                
                Component {
                    id: deviceItemComponent
                    Item {
                        id: deviceItem
                        anchors.leftMargin: 5
                        anchors.left: parent.left
                        anchors.right: parent.right
                        height: 30
                        
                        Loader { source: "DeviceItemView.qml" }
                        
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
                    anchors.fill: parent
                    spacing: 3
                    clip: true
                    focus: true
                    model: vmWorkspace.vmDevices
                    delegate: deviceItemComponent
                    highlight: Rectangle {
                        anchors.left: parent.left
                        anchors.right: parent.right
                        color: "lightsteelblue"
                        radius: 5
                    }
                }
            }
        }
    }
    
    Rectangle {
        Layout.fillWidth: true
        Layout.minimumWidth: 400
        Layout.preferredWidth: 500
        Layout.preferredHeight: workspaceLoaderArea.height-10
    }
}