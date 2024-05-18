import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root
    property var model
    property real desiredWidth
    property real desiredHeight
    property Component listComponent
    property Component detailComponent

    property bool detailed: false
    property int selectedIndex: -1

    readonly property real detailsVCenterOffset: 144
    readonly property real expectedListItemWidth: 350
    readonly property real expectedDetailsWidth: desiredWidth - (expectedListItemWidth + 48)

    /*
    Rectangle {
        anchors.fill: parent
        color: Qt.rgba(1.0, 1.0, 1.0, 0.03)
    }*/

    Loader {
        id: listLoader
        anchors.left: parent.left
        anchors.leftMargin: detailed ? 0 : root.desiredWidth / 2
        anchors.verticalCenter: parent.verticalCenter
        width: root.expectedListItemWidth
        height: Math.min(root.desiredHeight, item ? item.contentHeight : 0)
        sourceComponent: listComponent

        transitions: Transition {
            AnchorAnimation { duration: 100 }
        }

        state: "initial"
        states: [
            State {
                name: "detailed"
                when: detailed
                AnchorChanges {
                    target: listLoader
                    anchors.left: root.left
                    anchors.horizontalCenter: undefined
                }
            },
            State {
                name: "initial"
                when: !detailed
                AnchorChanges {
                    target: listLoader
                    anchors.left: undefined
                    anchors.horizontalCenter: root.horizontalCenter
                }
            }
        ]
    }

    Loader {
        active: detailed
        width: expectedDetailsWidth
        anchors.right: parent.right
        anchors.rightMargin: 16
        anchors.top: parent.verticalCenter
        anchors.topMargin: -detailsVCenterOffset
        height: item ? item.implicitHeight : 0
        sourceComponent: detailComponent
    }

    function selectPrevOrNext(step) {
        const listView = listLoader.item
        if (listView) {
            let index = selectedIndex + step
            if (index < 0) {
                index = listView.count - 1
            } else if (index > listView.count - 1) {
                index = 0
            }
            selectedIndex = index
            UI.ui.playSwitchSound()
        }
    }
}