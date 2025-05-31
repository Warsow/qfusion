import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root
    readonly property var availableRegularCrosshairs: UI.hudCommonDataModel.getAvailableRegularCrosshairs()
    readonly property var availableStrongCrosshairs: UI.hudCommonDataModel.getAvailableStrongCrosshairs()
    readonly property bool drawNativeParts: root.StackView.view && !root.StackView.view.busy
    readonly property real innerPaneMargin: 20.0
    readonly property real innerPaneWidth: 0.5 * root.width - innerPaneMargin

    ColumnLayout {
        width: 0.66 * root.width
        anchors.centerIn: parent
        spacing: 12

        SettingsGroupHeaderRow {
            text: "Input settings"
        }

        SettingsRow {
            text: "Mouse sensitivity"
            CVarAwareSliderWithBox {
                from: 0.1
                to: 10.0
                cvarName: "sensitivity"
            }
        }

        SettingsRow {
            text: "Smooth mouse"
            Layout.topMargin: -12
            CVarAwareCheckBox {
                cvarName: "m_filter"
            }
        }

        SettingsGroupHeaderRow {
            text: "Crosshair settings"
        }

        SettingsRow {
            text: "Crosshair damage color"
            CVarAwareColorPicker {
                cvarName: "cg_crosshairDamageColor"
            }
        }

        SettingsRow {
            text: "Additional strong crosshair"
            CVarAwareComboBox {
                id: strongComboBox
                knownHeadingsAndValues: [["(none)"].concat(availableStrongCrosshairs), [""].concat(availableStrongCrosshairs)]
                cvarName: "cg_strongCrosshairName"
            }
        }

        SettingsRow {
            text: "Strong crosshair color"
            CVarAwareColorPicker {
                id: strongColorPicker
                cvarName: "cg_strongCrosshairColor"
            }
        }

        SettingsRow {
            text: "Strong crosshair size"
            CVarAwareSliderWithBox {
                id: strongSizeSlider
                from: UI.ui.minStrongCrosshairSize
                to: UI.ui.maxStrongCrosshairSize
                stepSize: UI.ui.crosshairSizeStep
                cvarName: "cg_strongCrosshairSize"
                fractionalPartDigits: 0
            }
        }

        CVarAwareCheckBox {
            id: separateCheckBox
            Layout.alignment: Qt.AlignHCenter
            text: "Separate per weapon"
            cvarName: "cg_separateWeaponCrosshairSettings"
        }

        Item {
            id: weaponsPane
            Layout.fillWidth: true
            Layout.preferredHeight: Math.max(weaponsList.height, controlsPane.height)
            opacity: 0.0
            Behavior on opacity { NumberAnimation { duration: 200 } }
            Component.onCompleted: opacity = 1.0

            property int selectedIndex: 0
            property string weaponShortName: UI.hudCommonDataModel.getWeaponShortName(weaponsPane.selectedIndex + 1)

            ListView {
                id: weaponsList
                width: innerPaneWidth
                height: contentHeight
                model: 10
                anchors.verticalCenter: parent.verticalCenter
                anchors.right: parent.horizontalCenter
                anchors.rightMargin: innerPaneMargin
                interactive: false
                delegate: UILabel {
                    width: innerPaneWidth
                    height: 36
                    horizontalAlignment: Qt.AlignRight
                    verticalAlignment: Qt.AlignVCenter
                    font.weight: Font.Bold
                    //font.capitalization: Font.AllUppercase
                    font.letterSpacing: mouseArea.containsMouse ? 2.0 : 1.25
                    Behavior on font.letterSpacing { NumberAnimation { duration: 67 } }
                    color: enabled ? ((mouseArea.containsMouse || weaponsPane.selectedIndex === index) ?
                        Material.accent : Material.foreground) : "grey"
                    opacity: enabled ? 1.0 : 0.5
                    text: UI.hudCommonDataModel.getWeaponFullName(index + 1)
                    enabled: separateCheckBox.checked
                    MouseArea {
                        id: mouseArea
                        anchors.verticalCenter: parent.verticalCenter
                        anchors.right: parent.right
                        width: parent.implicitWidth
                        height: parent.height
                        hoverEnabled: true
                        onClicked: {
                            UI.ui.playSwitchSound()
                            weaponsPane.selectedIndex = index
                        }
                    }
                }
            }

            ColumnLayout {
                id: controlsPane
                anchors.verticalCenter: parent.verticalCenter
                anchors.left: parent.horizontalCenter
                anchors.leftMargin: innerPaneMargin + 12
                width: innerPaneWidth
                spacing: 8

                Rectangle {
                    Layout.preferredWidth: 172
                    Layout.preferredHeight: 172
                    Layout.alignment: Qt.AlignLeft
                    Layout.bottomMargin: 32
                    color: Qt.rgba(1.0, 1.0, 1.0, 0.07)

                    NativelyDrawnImage {
                        visible: drawNativeParts
                        anchors.centerIn: parent
                        desiredSize: Qt.size(strongSizeSlider.value, strongSizeSlider.value)
                        borderWidth: 1
                        materialName: strongComboBox.currentIndex < 0 ? "" :
                            UI.hudCommonDataModel.getStrongCrosshairFilePath(strongComboBox.values[strongComboBox.currentIndex])
                        useOutlineEffect: true
                        fitSizeForCrispness: true
                        color: strongColorPicker.selectedColor
                    }

                    NativelyDrawnImage {
                        visible: drawNativeParts
                        anchors.centerIn: parent
                        desiredSize: Qt.size(regularSizeSlider.value, regularSizeSlider.value)
                        borderWidth: 1
                        materialName: regularComboBox.currentIndex < 0 ? "" :
                            UI.hudCommonDataModel.getRegularCrosshairFilePath(regularComboBox.values[regularComboBox.currentIndex])
                        useOutlineEffect: true
                        fitSizeForCrispness: true
                        color: regularColorPicker.selectedColor
                    }
                }

                CVarAwareComboBox {
                    id: regularComboBox
                    knownHeadingsAndValues: [["(none)"].concat(availableRegularCrosshairs), [""].concat(availableRegularCrosshairs)]
                    cvarName: separateCheckBox.checked ? "cg_crosshairName_" + weaponsPane.weaponShortName : "cg_crosshairName"
                }

                CVarAwareSliderWithBox {
                    id: regularSizeSlider
                    Layout.alignment: Qt.AlignLeft
                    cvarName: separateCheckBox.checked ? "cg_crosshairSize_" + weaponsPane.weaponShortName : "cg_crosshairSize"
                    from: UI.ui.minRegularCrosshairSize
                    to: UI.ui.maxRegularCrosshairSize
                    stepSize: UI.ui.crosshairSizeStep
                    fractionalPartDigits: 0
                }

                CVarAwareColorPicker {
                    id: regularColorPicker
                    Layout.alignment: Qt.AlignLeft
                    cvarName: separateCheckBox.checked ? "cg_crosshairColor_" + weaponsPane.weaponShortName : "cg_crosshairColor"
                }
            }
        }
    }
}
