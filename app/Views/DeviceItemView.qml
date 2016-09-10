import QtQuick 2.0
import QtQuick.Controls 1.0
import QtQuick.Layouts 1.0


ColumnLayout {
    spacing: 1
    anchors.fill: parent
    
    Text {
        id: deviceName
        Layout.fillWidth: true
        Layout.fillHeight: true
        font.pointSize: 12
        text: modelData.vmDeviceName
        color: deviceItem.ListView.isCurrentItem ? "red" : "black"
    }
    
    Component {
        id: componentItemComponent
        Text {
            id: componentItem
            anchors.leftMargin: 25
            anchors.left: parent.left
            anchors.right: parent.right
            height: 20
            text: "component x."
        }
    }
    
    ListView {
        Layout.fillWidth: true
        Layout.fillHeight: true
        id: componentsListView
        anchors.fill: parent
        spacing: 3
        clip: true
        focus: false
        model: modelData.vmComponents
        delegate: componentItemComponent
    }
}