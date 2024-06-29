import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root
    property string title
    property var model
    property color baseColor: "black"

    property int playersPerRow: 3
    property int playersInFirstRow: playersPerRow
    readonly property int numPlayers: model ? model.length : 0

    visible: numPlayers
    implicitHeight: visible ?
        (label.anchors.topMargin + label.implicitHeight + label.anchors.topMargin +
        column.anchors.topMargin + column.implicitHeight + column.anchors.bottomMargin + 8) : 0


    Rectangle {
        anchors.fill: parent
        color: baseColor
        opacity: 0.5
    }

    Label {
        id: label
        anchors.top: parent.top
        anchors.topMargin: 8
        anchors.horizontalCenter: parent.horizontalCenter
        font.family: UI.ui.headingFontFamily
        font.capitalization: Font.AllUppercase
        font.pointSize: UI.scoreboardFontSize
        font.letterSpacing: UI.scoreboardLetterSpacing
        font.weight: Font.Bold
        text: root.title
        style: Text.Raised
    }

    ColumnLayout {
        id: column
        width: parent.width - 8
        anchors.top: label.bottom
        anchors.topMargin: 16 + 2
        anchors.horizontalCenter: parent.horizontalCenter
        spacing: 8

        Repeater {
            id: rowsRepeater
            model: {
                if (!numPlayers) {
                    0
                } else if (numPlayers <= playersInFirstRow) {
                    1
                } else {
                    1 + Math.floor((numPlayers - playersInFirstRow) / playersPerRow) + 1
                }
            }
            delegate: Row {
                Layout.alignment: Qt.AlignHCenter
                readonly property int rowIndex: index
                spacing: 8
                Repeater {
                    model: {
                        if (rowIndex === 0) {
                            Math.min(numPlayers, playersInFirstRow)
                        } else if(rowIndex + 1 != rowsRepeater.count) {
                            playersPerRow
                        } else {
                            (numPlayers - playersInFirstRow) % playersPerRow
                        }
                    }
                    delegate: Item {
                        id: playerItem
                        readonly property int listIndex: {
                            if (rowIndex === 0) {
                                index
                            } else {
                                playersInFirstRow + (rowIndex - 1) * playersPerRow + index
                            }
                        }
                        // Caution: This binding gets broken on a first procedural assignment
                        width: nameLabel.width + pingLabel.width + 8
                        height: Math.max(nameLabel.height, pingLabel.height)
                        property real pendingWidth: width
                        function updateWidth() {
                            const newWidth = nameLabel.width + pingLabel.width + 8
                            if (newWidth > width) {
                                shrinkAnim.stop()
                                width = newWidth
                            } else if (newWidth < width) {
                                shrinkAnim.stop()
                                pendingWidth = newWidth
                                shrinkAnim.start()
                            }
                        }
                        SequentialAnimation {
                            id: shrinkAnim
                            PauseAnimation { duration: 250 }
                            SmoothedAnimation {
                                target: playerItem
                                property: "width"
                                to: playerItem.pendingWidth
                                duration: 250
                            }
                        }
                        Label {
                            id: nameLabel
                            anchors.left: parent.left
                            anchors.bottom: parent.bottom
                            width: implicitWidth + 8
                            horizontalAlignment: Qt.AlignLeft
                            font.weight: Font.Bold
                            font.pointSize: UI.scoreboardFontSize
                            font.letterSpacing: UI.scoreboardLetterSpacing
                            style: Text.Raised
                            text: root.model[listIndex]["name"]
                            onWidthChanged: playerItem.updateWidth()
                        }
                        UILabel {
                            id: pingLabel
                            anchors.right: parent.right
                            anchors.bottom: parent.bottom
                            width: implicitWidth + 12
                            horizontalAlignment: Qt.AlignLeft
                            font.weight: Font.Bold
                            font.pointSize: UI.scoreboardFontSize
                            font.letterSpacing: UI.scoreboardLetterSpacing
                            style: Text.Raised
                            text: root.model[listIndex]["ping"]
                            onWidthChanged: playerItem.updateWidth()
                        }
                    }
                }
            }
        }
    }
}