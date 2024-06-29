import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

InGameCallvoteScalarSelector {
    currentValue: selectedVoteCurrent
    delegate: UITextField {
        id: textField
        horizontalAlignment: Qt.AlignHCenter
        Material.theme: activeFocus ? Material.Light : Material.Dark
        // TODO: Servers should supply arg bounds/limitations
        validator: IntValidator {
            bottom: 0; top: 99
        }
        // TODO: Why does not requesting active focus work
        Component.onCompleted: text = currentValue
        onTextChanged: chosenValue = text
    }
}