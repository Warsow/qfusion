import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

ListView {
    id: root
    interactive: false

    property bool showEmptyListHeader: false

    header: Item {
        width: root.width
        height: visible ? 32 : 0
        visible: typeof(model) !== "undefined" || showEmptyListHeader

        ServerBrowserDataLabel {
            anchors.left: parent.left
            anchors.leftMargin: 8
            anchors.verticalCenter: parent.verticalCenter
            font.weight: Font.DemiBold
            text: "Name"
        }

        Row {
            anchors.right: parent.right
            anchors.rightMargin: 8
            anchors.verticalCenter: parent.verticalCenter
            spacing: 8

            ServerBrowserDataLabel {
                width: 48
                horizontalAlignment: Qt.AlignHCenter
                font.weight: Font.DemiBold
                text: "Score"
            }

            ServerBrowserDataLabel {
                width: 48
                horizontalAlignment: Qt.AlignHCenter
                font.weight: Font.DemiBold
                text: "Ping"
            }
        }
    }

    delegate: Item {
        id: delegateItem
        width: root.width
        height: 24
        Component.onDestruction: UI.ui.ensureObjectDestruction(delegateItem)

        ServerBrowserDataLabel {
            text: modelData["name"]
            width: root.width - scoreAndPingRow.width - 8

            anchors.left: parent.left
            anchors.leftMargin: 8
            anchors.verticalCenter: parent.verticalCenter
        }

        Row {
            id: scoreAndPingRow
            anchors.right: parent.right
            anchors.rightMargin: 8
            anchors.verticalCenter: parent.verticalCenter
            spacing: 8

            ServerBrowserDataLabel {
                width: 48
                horizontalAlignment: Qt.AlignHCenter
                text: modelData["score"]
            }

            ServerBrowserDataLabel {
                width: 48
                horizontalAlignment: Qt.AlignHCenter
                text: UI.ui.formatPing(modelData["ping"])
            }
        }
    }
}