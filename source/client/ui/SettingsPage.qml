import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    readonly property var handleKeyEvent: stackView.currentItem["handleKeyEvent"]

    readonly property bool drawNativePart: StackView.view && !StackView.view.busy && !stackView.busy

    Component {
        id: playerSettingsComponent
        PlayerSettings {
            drawNativePart: root.drawNativePart
        }
    }
    Component {
        id: teamsSettingsComponent
        TeamsSettings {
            drawNativePart: root.drawNativePart
        }
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
        MouseSettings {
            drawNativePart: root.drawNativePart
        }
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

    StackView.onStatusChanged: appearDisappearHelper.expandAndHideIfDeactivating(StackView.status)

    CarouselTabBar {
        id: tabBar
        enabled: !UI.ui.hasPendingCVarChanges
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        AppearDisappearHelper { id: appearDisappearHelper }

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

    // TODO: Extract a generic component for SwipeView-like stack views
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
        height: UI.regularSlantedButtonHeight + 2 * 16
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
                height: UI.regularSlantedButtonHeight + 16
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