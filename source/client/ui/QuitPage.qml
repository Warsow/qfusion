import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: quitPage
    property var backTrigger

    // TODO: Get rid of this Popup (requires rewriting dimming/blurring overlay logic of the root item)
    Popup {
        id: popup
        modal: true
        focus: true
        dim: false
        closePolicy: Popup.CloseOnEscape
        anchors.centerIn: parent
        width: UI.desiredPopupContentWidth
        height: width

        function openSelf() {
            popup.parent = rootItem.windowContentItem
            rootItem.setOrUpdatePopupMode()
            popup.open()
        }

        function closeSelf() {
            if (opened) {
                UI.ui.playBackSound()
                popup.close()
                quitPage.backTrigger()
            }
        }

        onAboutToHide: rootItem.resetPopupMode()

        background: null

        contentItem: ConfirmationItem {
            id: confirmationItem
            titleText: "Quit the game?"
            numButtons: 2
            buttonTexts: ["Quit", "Go back"]
            buttonFocusStatuses: [false, true]
            buttonEnabledStatuses: [true, true]
            onButtonClicked: {
                if (buttonIndex === 0) {
                    UI.ui.quit()
                } else {
                    popup.closeSelf()
                }
            }
            onButtonActiveFocusChanged: {
                const newStatuses = [...buttonFocusStatuses]
                newStatuses[buttonIndex] = buttonActiveFocus
                buttonFocusStatuses = newStatuses
            }
            Component.onCompleted: confirmationItem.forceActiveFocus()
            // Popup key handling cannot be intercepted by regular facilities declared in MenuRootItem.qml
            Keys.onPressed: {
                if (event.key === Qt.Key_Escape) {
                    event.accepted = true
                    popup.closeSelf()
                }
            }
        }
    }

    Component.onCompleted: popup.openSelf()
    Component.onDestruction: popup.closeSelf()
}