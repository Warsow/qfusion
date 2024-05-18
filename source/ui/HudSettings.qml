import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

StackView {
    id: root
    initialItem: settingsComponent

    readonly property var listOfRegularHuds: UI.regularHudEditorModel.existingHuds
    readonly property var listOfMiniviewHuds: UI.miniviewHudEditorModel.existingHuds

    function startHudEditorWizard() {
        clear(StackView.Immediate)
        push(editorWizardComponent)
    }

    function closeHudEditorWizard() {
        clear(StackView.Immediate)
        push(settingsComponent)
    }

    function handleKeyEvent(event) {
        if (currentItem.hasOwnProperty("handleKeyEvent")) {
            return currentItem["handleKeyEvent"](event)
        }
        return false
    }

    Component {
        id: settingsComponent
        Item {
            ColumnLayout {
                anchors.centerIn: parent
                width: 0.67 * parent.width
                spacing: -2

                SettingsGroupHeaderRow { text: "View" }

                SettingsRow {
                    text: "Show zoom effect"
                    CVarAwareCheckBox { cvarName: "cg_showZoomEffect" }
                }
                SettingsRow {
                    text: "Field of view while zooming"
                    CVarAwareSliderWithBox {
                        cvarName: "zoomfov"
                        from: 5.0; to: 75.0;
                        fractionalPartDigits: 0
                    }
                }
                SettingsRow {
                    text: "Field of view"
                    CVarAwareSliderWithBox {
                        cvarName: "fov"
                        from: 80.0; to: 140.0
                        fractionalPartDigits: 0
                    }
                }

                SettingsGroupHeaderRow { text: "HUD" }

                SettingsRow {
                    text: "Regular"
                    CVarAwareComboBox {
                        knownHeadingsAndValues: [listOfRegularHuds, listOfRegularHuds]
                        cvarName: "cg_regularHud"
                    }
                }

                SettingsRow {
                    text: "Miniview"
                    CVarAwareComboBox {
                        knownHeadingsAndValues: [listOfMiniviewHuds, listOfMiniviewHuds]
                        cvarName: "cg_miniviewHud"
                    }
                }

                SlantedButton {
                    Layout.preferredWidth: UI.neutralCentralButtonWidth
                    Layout.margins: 16
                    Layout.alignment: Qt.AlignHCenter
                    text: "Edit HUDs"
                    font.capitalization: Font.MixedCase
                    labelHorizontalCenterOffset: 0
                    leftBodyPartSlantDegrees: -0.5 * UI.buttonBodySlantDegrees
                    rightBodyPartSlantDegrees: +0.5 * UI.buttonBodySlantDegrees
                    textSlantDegrees: 0
                    highlighted: true
                    //Material.accent: Qt.lighter(Material.background, 1.1)
                    onClicked: {
                        UI.ui.playForwardSound()
                        root.startHudEditorWizard()
                    }
                }

                SettingsGroupHeaderRow { text: "HUD elements" }

                SettingsRow {
                    text: "Show FPS counter"
                    CVarAwareCheckBox { cvarName: "cg_showFPS" }
                }
                SettingsRow {
                    text: "Show player speed"
                    CVarAwareCheckBox { cvarName: "cg_showSpeed" }
                }
                SettingsRow {
                    text: "Show minimap"
                    CVarAwareCheckBox { cvarName: "cg_showMiniMap" }
                }
                SettingsRow {
                    text: "Show awards"
                    CVarAwareCheckBox { cvarName: "cg_showAwards" }
                }
                SettingsRow {
                    text: "Show frags feed"
                    CVarAwareCheckBox { cvarName: "cg_showFragsFeed" }
                }
                SettingsRow {
                    text: "Show message feed"
                    CVarAwareCheckBox { cvarName: "cg_showMessageFeed" }
                }
                SettingsRow {
                    text: "Show pressed keys"
                    CVarAwareCheckBox { cvarName: "cg_showPressedKeys" }
                }
                SettingsRow {
                    text: "Show player names"
                    CVarAwareCheckBox { cvarName: "cg_showPlayerNames" }
                }
                SettingsRow {
                    text: "Show status of teammates you look at"
                    CVarAwareCheckBox { cvarName: "cg_showPointedPlayer" }
                }
                SettingsRow {
                    text: "Show teammates info & locations"
                    CVarAwareCheckBox { cvarName: "cg_showTeamInfo" }
                }
                SettingsRow {
                    text: "Show teammate beacons through walls"
                    CVarAwareCheckBox { cvarName: "cg_showTeamInfo" }
                }
            }
        }
    }

    Component {
        id: editorWizardComponent
        HudEditorWizard {
            onExitRequested: closeHudEditorWizard()
        }
    }
}

