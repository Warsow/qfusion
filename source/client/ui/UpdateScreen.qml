import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    ConfirmationItem {
        anchors.centerIn: parent
        titleText: "Warsow update is available"
        numButtons: 2
        buttonTexts: ["Skip", "Download"]
        buttonFocusStatuses: [false, true]
        buttonEnabledStatuses: [true, true]
        onButtonClicked: {
            if (buttonIndex === 0) {
                UI.ui.hideUpdates()
            } else {
                UI.ui.downloadUpdates();
            }
        }
        onButtonActiveFocusChanged: {
            const newStatuses = [...buttonFocusStatuses]
            newStatuses[buttonIndex] = buttonActiveFocus
            buttonFocusStatuses = newStatuses
        }
        contentComponent: UILabel {
            text: "New version is " + UI.ui.availableUpdateVersion
        }
        Component.onCompleted: forceActiveFocus()
    }
}