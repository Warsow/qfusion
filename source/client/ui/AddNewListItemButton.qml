import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

FocusScope {
    id: root
    implicitWidth: 300
    implicitHeight: Math.max(addButton.implicitHeight, textField.implicitHeight, clearButton.implicitHeight)

    property int maxHudNameLength

    signal additionRequested(string text)

    function close() {
        textField.text = ""
        textField.visible = false
        textField.width = 0
        addButton.visible = true
    }

    ToolButton {
        id: addButton
        icon.source: "qrc:/Add.svg"
        icon.width: 16; icon.height: 16
        Material.theme: Material.Dark
        anchors.centerIn: parent
        onHoveredChanged: {
            if (hovered) {
                UI.ui.playHoverSound()
            }
        }
        onClicked: {
            UI.ui.playSwitchSound()
            visible = false
            textField.visible = true
            textField.width = root.width - 64
            textField.forceActiveFocus()
        }
    }

    UITextField {
        id: textField
        Material.theme: activeFocus ? Material.Light : Material.Dark
        visible: false
        anchors.centerIn: parent
        horizontalAlignment: Qt.AlignHCenter
        width: 0
        Behavior on width { SmoothedAnimation { duration: 100 } }
        font.weight: Font.Bold
        font.pointSize: UI.labelFontSize
        font.letterSpacing: UI.labelLetterSpacing
        font.capitalization: Font.AllUppercase
        maximumLength: root.maxHudNameLength
        validator: RegExpValidator { regExp: /[0-9a-zA-Z_]+/ }
        onEditingFinished: {
            if (text.length > 0) {
                root.additionRequested(text.toLowerCase())
            }
            close()
        }
        onActiveFocusChanged: {
            if (!activeFocus) {
                close()
            }
        }
    }

    ToolButton {
        id: clearButton
        icon.source: "qrc:/Close.svg"
        icon.width: 12; icon.height: 12
        visible: textField.text.length > 0
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        onHoveredChanged: {
            if (hovered) {
                UI.ui.playHoverSound()
            }
        }
        onClicked: close()
    }
}