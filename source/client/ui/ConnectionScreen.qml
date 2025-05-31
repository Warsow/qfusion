import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    StackView {
        id: stackView
        anchors.fill: parent
        initialItem: logoComponent
        replaceEnter: null
        replaceExit: null
        onCurrentItemChanged: {
            if (currentItem) {
                currentItem.forceActiveFocus()
            }
        }
    }

    Component {
        id: logoComponent
        Item {
            Image {
                anchors.centerIn: parent
                fillMode: Image.PreserveAspectFit
			    source: "image://wsw/gfx/ui/loadinglogo"
            }
        }
    }

    Component {
        id: dialogComponent
        //property real scale: 0.0
        ConfirmationItem {
            id: dialogItem
            property string password
            readonly property bool enableRetryButton:  UI.ui.reconnectBehaviour === UISystem.OfUserChoice ||
                (UI.ui.reconnectBehaviour === UISystem.RequestPassword && dialogItem.password.length)
            titleText: UI.ui.droppedConnectionTitle
            numButtons: UI.ui.reconnectBehaviour === UISystem.DontReconnect ? 1 : 2
            buttonTexts: numButtons === 1 ? ["Ok"] : ["Go back", "Retry"]
            buttonEnabledStatuses: numButtons === 1 ? [true] : [true, dialogItem.enableRetryButton]
            buttonFocusStatuses: numButtons === 1 ? [true] : [false, dialogItem.enableRetryButton]
            contentToButtonsKeyNavigationTargetIndex: enableRetryButton ? 1 : 0
            onButtonClicked: {
                if (buttonIndex === 0) {
                    UI.ui.playBackSound()
                    UI.ui.stopReactingToDroppedConnection()
                } else {
                    UI.ui.playForwardSound()
                    if (UI.ui.reconnectBehaviour === UISystem.RequestPassword) {
                        UI.ui.reconnectWithPassword(dialogItem.password)
                    } else {
                        UI.ui.reconnect()
                    }
                }
            }
            onEnableRetryButtonChanged: {
                if (numButtons > 1) {
                    // Don't rely on bindings that could get broken
                    buttonEnabledStatuses = [true, dialogItem.enableRetryButton]
                    buttonFocusStatuses   = [false, false]
                }
            }
            onButtonActiveFocusChanged: {
                const newStatuses = [...buttonFocusStatuses]
                if (buttonIndex === 0) {
                    newStatuses[0] = buttonActiveFocus
                } else {
                    newStatuses[1] = buttonActiveFocus && dialogItem.enableRetryButton
                }
                // Force re-evaluation of all statuses
                buttonFocusStatuses = newStatuses
            }
            contentComponent: ColumnLayout {
                readonly property bool focusable: passwordInput.visible
                spacing: 8

                onActiveFocusChanged: {
                    if (passwordInput.visible) {
                        passwordInput.forceActiveFocus()
                    }
                }

                UILabel {
                    id: descLabel
                    Layout.preferredWidth: UI.desiredPopupContentWidth
                    horizontalAlignment: Text.AlignHCenter
                    wrapMode: Text.WordWrap
                    maximumLineCount: 5
                    elide: Qt.ElideRight
                    lineHeight: 1.25
                    text: UI.ui.droppedConnectionMessage
                }

                UITextField {
                    id: passwordInput
                    Layout.alignment: Qt.AlignHCenter
                    visible: UI.ui.reconnectBehaviour === UISystem.RequestPassword
                    enabled: visible
                    Material.theme: activeFocus ? Material.Light : Material.Dark
                    horizontalAlignment: Qt.AlignHCenter
                    maximumLength: 16
                    echoMode: TextInput.Password
                    Keys.onEnterPressed: {
                        if (dialogItem.password.length) {
                            UI.ui.reconnectWithPassword(dialogItem.password)
                        }
                    }
                    onTextEdited: dialogItem.password = text
                }
            }
        }
    }

    Connections {
        target: UI.ui
        onIsReactingToDroppedConnectionChanged: {
            if (UI.ui.isReactingToDroppedConnection) {
                UI.ui.playBackSound()
                stackView.clear(StackView.Immediate)
                stackView.push(dialogComponent, null, StackView.ReplaceTransition)
            } else {
                stackView.clear(StackView.Immediate)
                stackView.push(logoComponent, null, StackView.Immediate)
            }
        }
    }

    Keys.onPressed: {
        if (event.key === Qt.Key_Escape) {
            UI.ui.playBackSound()
            UI.ui.stopReactingToDroppedConnection()
            UI.ui.disconnect()
        }
    }
}