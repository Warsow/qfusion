import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    readonly property var handleKeyEvent: stackView.currentItem["handleKeyEvent"]

    Component {
        id: playerSettingsComponent
        PlayerSettings {}
    }
    Component {
        id: teamsSettingsComponent
        TeamsSettings {}
    }
    Component {
        id: graphicsSettingsComponent
        GraphicsSettings {}
    }
    Component {
        id: soundSettingsComponent
        SoundSettings {}
    }
    Component {
        id: mouseSettingsComponent
        MouseSettings {}
    }
    Component {
        id: keyboardSettingsComponent
        KeyboardSettings {}
    }
    Component {
        id: hudSettingsComponent
        HudSettings {}
    }

    // A safety guard
    Component.onDestruction: UI.ui.rollbackPendingCVarChanges()

    CarouselTabBar {
        id: tabBar
        enabled: !UI.ui.hasPendingCVarChanges
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        onCurrentIndexChanged: stackView.replace(model[currentIndex]["component"])

        model: [
            {"text": "Player", "component" : playerSettingsComponent},
            {"text": "Teams", "component" : teamsSettingsComponent },
            {"text": "Graphics", "component" : graphicsSettingsComponent },
            {"text": "Sound", "component" : soundSettingsComponent },
            {"text": "Mouse", "component" : mouseSettingsComponent },
            {"text": "Keyboard", "component" : keyboardSettingsComponent },
            {"text": "HUD", "component" : hudSettingsComponent },
        ]
    }

    StackView {
        id: stackView
        anchors.top: tabBar.bottom
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        initialItem: playerSettingsComponent
        clip: false
    }

    Loader {
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 16
        anchors.horizontalCenter: parent.horizontalCenter
        width: 2 * parent.width / 3
        height: 72
        active: UI.ui.hasPendingCVarChanges
        sourceComponent: applyChangesComponent
    }

    Component {
        id: applyChangesComponent

        Item {
            id: applyChangesPane

            Rectangle {
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.verticalCenter: parent.verticalCenter
                radius: 4
                width: parent.width - 16
                height: parent.height - 16
                color: UI.ui.colorWithAlpha(Qt.darker(Material.background, 1.25), 0.67)
            }

            SlantedLeftSecondaryButton {
                anchors {
                    verticalCenter: parent.verticalCenter
                    right: parent.horizontalCenter
                    rightMargin: 0.5 * UI.minAcceptRejectSpacing
                }
                text: "Revert"
                width: implicitWidth
                onClicked: {
                    UI.ui.playBackSound()
                    UI.ui.rollbackPendingCVarChanges()
                }
            }

            SlantedRightPrimaryButton {
                highlighted: true
                anchors {
                    verticalCenter: parent.verticalCenter
                    left: parent.horizontalCenter
                    leftMargin: 0.5 * UI.minAcceptRejectSpacing
                }
                width: implicitWidth
                text: "Accept"
                onClicked: {
                    UI.ui.playForwardSound()
                    UI.ui.commitPendingCVarChanges()
                }
            }
        }
    }
}