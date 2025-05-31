import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root
    ColumnLayout {
        anchors.centerIn: parent
        width: 0.67 * root.width
        spacing: 4

        SettingsRow {
            text: "Nickname"
            CVarAwareTextField { cvarName: "name"; font.weight: Font.Medium }
        }

        SettingsRow {
            text: "Clan"
            CVarAwareTextField { cvarName: "clan"; font.weight: Font.Medium }
        }

        SettingsGroupHeaderRow {
            text: "Your player model"
            Layout.topMargin: 20
        }

        CVarAwareModelSelector {
            Layout.preferredWidth: 480
            Layout.preferredHeight: 480
            Layout.alignment: Qt.AlignHCenter
            modelColor: colorPicker.selectedColor
            fullbright: fullbrightCheckBox.checked
            drawNativePart: root.StackView.view && !root.StackView.view.busy
            defaultModel: UI.ui.defaultPlayerModel
            cvarName: "model"
        }

        CVarAwareColorPicker {
            id: colorPicker
            Layout.alignment: Qt.AlignHCenter
            Layout.topMargin: 16
            Layout.bottomMargin: 16
            cvarName: "color"
        }

        SettingsRow {
            text: "Fullbright"
            CVarAwareCheckBox {
                id: fullbrightCheckBox
                cvarName: "skin"
                function fromNative(value) { return value.toLowerCase() == "fullbright" }
                function toNative(value) { return value ? "fullbright" : "default" }
            }
        }
    }
}
