import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    Item {
        id: pane
        width: Math.min(parent.width, 720)
        height: 112

        layer.enabled: true
        layer.effect: ElevationEffect { elevation: 64 }

        Component.onCompleted: {
            x = 0.5 * (parent.width - width)
            y = 0.75 * parent.height
        }

        Connections {
            target: UI.ui
            onHudOccludersRetrievalRequested: UI.ui.supplyHudOccluder(pane)
        }

        MouseArea {
            id: mouseArea
            anchors.fill: parent
            drag.target: parent
            onClicked: {
                if (mouse.y >= progressBar.y - 4 && mouse.y <= progressBar.y + 12) {
                    if (mouse.x >= progressBar.x && mouse.x <= progressBar.x + progressBar.width) {
                        UI.ui.playSwitchSound()
                        UI.demoPlayer.seek((mouse.x - progressBar.x) / progressBar.width)
                    }
                }
            }
        }

        Rectangle {
            width: parent.width
            height: parent.height
            anchors.bottom: parent.bottom
            color: Material.background
            opacity: (UI.ui.isShowingScoreboard || UI.hudCommonDataModel.hasTiledMiniviews) ? 0.9 : 0.7
            radius: 6
        }

        RowLayout {
            id: nameLabel
            anchors.top: parent.top
            anchors.topMargin: 8
            anchors.horizontalCenter: parent.horizontalCenter
            width: progressBar.width

            UILabel {
                font.family: UI.ui.headingFontFamily
                font.weight: Font.Bold
                font.capitalization: Font.AllUppercase
                style: Text.Raised
                text: UI.demoPlayer.mapName + " - " + UI.demoPlayer.gametype
            }

            UILabel {
                Layout.fillWidth: true
                elide: Text.ElideMiddle
                horizontalAlignment: Qt.AlignHCenter
                font.family: UI.ui.headingFontFamily
                font.capitalization: Font.AllUppercase
                font.weight: Font.Bold
                style: Text.Raised
                text: UI.demoPlayer.demoName
            }

            UILabel {
                font.family: UI.ui.numbersFontFamily
                font.letterSpacing: 1
                font.weight: Font.Bold
                style: Text.Raised
                text: UI.demoPlayer.timestamp
            }
        }

        ProgressBar {
            id: progressBar
            anchors.top: nameLabel.bottom
            anchors.topMargin: 8
            width: parent.width - 32
            anchors.horizontalCenter: parent.horizontalCenter

            indeterminate: UI.demoPlayer.isPaused

            hoverEnabled: true
            onHoveredChanged: {
                if (hovered) {
                    UI.ui.playHoverSound()
                }
            }

            from: 0.0
            to: UI.demoPlayer.duration
            value: UI.demoPlayer.progress
        }

        UILabel {
            anchors.left: progressBar.left
            anchors.top: progressBar.bottom
            anchors.topMargin: 8
            text: UI.demoPlayer.formatDuration(UI.demoPlayer.progress)
            font.family: UI.ui.numbersFontFamily
            font.weight: Font.Bold
            font.letterSpacing: 1
            style: Text.Raised
        }

        UILabel {
            anchors.right: progressBar.right
            anchors.top: progressBar.bottom
            anchors.topMargin: 8
            text: UI.demoPlayer.formatDuration(UI.demoPlayer.duration)
            font.family: UI.ui.numbersFontFamily
            font.weight: Font.Bold
            font.letterSpacing: 1
            style: Text.Raised
        }

        RowLayout {
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.top: progressBar.bottom
            anchors.topMargin: 12

            Button {
                flat: true
                display: AbstractButton.TextOnly
                Layout.preferredWidth: 64
                text: "\u23EF"
                font.family: UI.ui.emojiFontFamily
                font.pointSize: 20
                onHoveredChanged: {
                    if (hovered) {
                        UI.ui.playHoverSound()
                    }
                }
                onClicked: {
                    UI.ui.playSwitchSound()
                    UI.demoPlayer.pause()
                }
            }

            Button {
                flat: true
                display: AbstractButton.TextOnly
                Layout.preferredWidth: 64
                text: "\u23F9"
                font.family: UI.ui.emojiFontFamily
                font.pointSize: 20
                onHoveredChanged: {
                    if (hovered) {
                        UI.ui.playHoverSound()
                    }
                }
                onClicked: {
                    UI.ui.playBackSound()
                    UI.demoPlayer.stop()
                }
            }
        }
    }
}