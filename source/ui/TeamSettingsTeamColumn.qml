import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

ColumnLayout {
    id: root

    property string teamName
    property string defaultModel
    property real modelSelectorWidth
    property real modelSelectorHeight
    property bool hasForceColorVar: false
    property bool drawNativePart

    spacing: 8

    Rectangle {
        Layout.fillWidth: true
        Layout.preferredHeight: 48
        color: Qt.rgba(colorPicker.selectedColor.r, colorPicker.selectedColor.g, colorPicker.selectedColor.b, 0.3)
        UILabel {
            anchors.centerIn: parent
            font.capitalization: Font.AllUppercase
            font.weight: Font.Black
            font.letterSpacing: 1
            text: teamName
        }
    }

    CVarAwareModelSelector {
        Layout.topMargin: 16
        Layout.alignment: Qt.AlignHCenter
        Layout.preferredWidth: root.modelSelectorWidth
        Layout.preferredHeight: root.modelSelectorHeight
        modelColor: colorPicker.selectedColor
        fullbright: fullbrightCheckBox.checked
        drawNativePart: root.drawNativePart
        defaultModel: root.defaultModel
        cvarName: "cg_team" + teamName + "Model"
    }

    CVarAwareColorPicker {
        id: colorPicker
        Layout.topMargin: 12
        Layout.alignment: Qt.AlignHCenter
        cvarName: "cg_team" + teamName + "Color"
    }

    CVarAwareCheckBox {
        id: fullbrightCheckBox
        Layout.topMargin: 20
        text: "Fullbright"
        Layout.alignment: Qt.AlignHCenter
        cvarName: "cg_team" + teamName + "Skin"
        function fromNative(value) { return value.toLowerCase() == "fullbright" }
        function toNative(value) { return value ? "fullbright" : "default" }
    }

    CVarAwareCheckBox {
        text: "Force this model for <b>" + teamName + "</b>"
        Layout.alignment: Qt.AlignHCenter
        cvarName: "cg_team" + teamName + "ModelForce"
    }

    Loader {
        active: hasForceColorVar
        Layout.alignment: Qt.AlignHCenter
        sourceComponent: CVarAwareCheckBox {
            text: "Force this color for <b>" + teamName + "</b>"
            cvarName: "cg_team" + teamName + "ColorForce"
        }
    }
}