import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root
    property var model

    readonly property int playersPerRow: 3
    readonly property int numPlayers: model ? model.length : 0

    implicitHeight: model ?
        (label.implicitHeight + label.anchors.topMargin + column.implicitHeight + column.anchors.topMargin) : 0

    ServerBrowserDataLabel {
        id: label
        anchors.top: parent.top
        anchors.topMargin: 12
        font.weight: Font.Bold
        anchors.horizontalCenter: parent.horizontalCenter
        text: "Spectators"
    }

    ColumnLayout {
        id: column
        spacing: 12

        anchors.top: label.bottom
        anchors.topMargin: 12
        anchors.left: parent.left
        anchors.leftMargin: 8
        anchors.right: parent.right
        anchors.rightMargin: 8

        Repeater {
            model: numPlayers ? Math.max(numPlayers / playersPerRow, 1) : 0
            delegate: Row {
                id: outerDelegate
                Component.onDestruction: UI.ui.ensureObjectDestruction(outerDelegate)
                readonly property int rowIndex: index
                Layout.alignment: Qt.AlignHCenter
                spacing: 12
                Repeater {
                    model: (rowIndex !== Math.floor(numPlayers / playersPerRow)) ?
                        playersPerRow : (numPlayers % playersPerRow)
                    delegate: Row {
                        id: innerDelegate
                        Component.onDestruction: UI.ui.ensureObjectDestruction(innerDelegate)
                        spacing: 8
                        readonly property int listIndex: rowIndex * playersPerRow + index
                        ServerBrowserDataLabel {
                            text: root.model[listIndex]["name"]
                        }
                        ServerBrowserDataLabel {
                            text: UI.ui.formatPing(root.model[listIndex]["ping"])
                        }
                    }
                }
            }
        }
    }
}