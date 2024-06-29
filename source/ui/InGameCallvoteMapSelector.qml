import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import net.warsow 2.6

Item {
    id: root
    property var currentValue
    property var chosenValue
    property var model

    readonly property bool canProceed: !!chosenValue

    readonly property string displayedString: {
        if (typeof(chosenValue) !== "undefined") {
            'Chosen <b>' + chosenValue + '</b> over <b>' + currentValue + '</b>'
        } else {
            '<b>' + currentValue + '</b> is the current map'
        }
    }

    readonly property int numCellsPerRow: 4

    GridView {
        id: levelshotGrid
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.horizontalCenter: parent.horizontalCenter
        width: parent.width - 16
        model: root.model
        cellWidth: levelshotGrid.width / numCellsPerRow
        cellHeight: levelshotGrid.width / numCellsPerRow
        clip: true

        delegate: MouseArea {
            id: levelshotMouseArea
            width: levelshotGrid.cellWidth
            height: levelshotGrid.cellHeight
            hoverEnabled: true

            layer.enabled: containsMouse
            layer.effect: ElevationEffect { elevation: placeholder.visible ? 8 : 64 }

            // TODO: Add a position timer

            onContainsMouseChanged: {
                if (containsMouse) {
                    UI.ui.playHoverSound()
                    levelshotGrid.positionViewAtIndex(index, GridView.Center)
                }
            }

            onClicked: {
                UI.ui.playSwitchSound()
                root.chosenValue = modelData["name"]
            }

            Image {
                id: image
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.verticalCenter: parent.verticalCenter
                width: levelshotMouseArea.containsMouse ? parent.width + 12 : parent.width
                height: levelshotMouseArea.containsMouse ? parent.height + 12 : parent.height
                source: "image://wsw/levelshots/" + modelData["name"]
                fillMode: Image.PreserveAspectCrop
                mipmap: true
                opacity: levelshotMouseArea.containsMouse ? 1.0 : 0.5
            }

            Rectangle {
                id: placeholder
                // TODO: This is a workaround. Check why image.status is considered Ready for missing images
                visible: !image.sourceSize.width && !image.sourceSize.height
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.verticalCenter: parent.verticalCenter
                width: image.width
                height: image.height
                opacity: levelshotMouseArea.containsMouse ? 0.5 : 0.1
                // TODO: Use a checkerboard pattern?
                color: Qt.rgba(1.0, 1.0, 1.0, 0.33)
            }

            UILabel {
                anchors.bottom: parent.bottom
                anchors.bottomMargin: 4
                anchors.horizontalCenter: parent.horizontalCenter
                horizontalAlignment: Qt.AlignHCenter
                width: parent.width
                elide: Qt.ElideMiddle
                color: levelshotMouseArea.containsMouse ? Material.accent : Material.foreground
                font.weight: Font.Medium
                style: Text.Raised
                text: modelData["name"]
            }

            Rectangle {
                anchors.fill: parent
                radius: 2
                color: "transparent"
                border.color: Material.accent
                border.width: levelshotMouseArea.containsMouse ? 2 : 0
            }
        }
    }
}