import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import QtGraphicalEffects 1.12
import net.warsow 2.6

Item {
    id: root

    RadialGradient {
        anchors.fill: parent
        gradient: Gradient {
            GradientStop { position: 0.0; color: Qt.darker(Material.background, 1.1) }
            GradientStop { position: 0.9; color: Qt.darker(Material.background, 1.9) }
        }
    }

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
            Logo {
                anchors.centerIn: parent
                width: parent.width
                height: implicitHeight
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
                    UI.ui.stopReactingToDroppedConnection()
                } else {
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
                    buttonFocusStatuses   = [false, dialogItem.enableRetryButton]
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

                Label {
                    id: descLabel
                    Layout.alignment: Qt.AlignHCenter
                    wrapMode: Text.WordWrap
                    maximumLineCount: 4
                    elide: Qt.ElideRight
                    lineHeight: 1.25
                    font.pointSize: 13
                    font.letterSpacing: 0.5
                    text: UI.ui.droppedConnectionMessage
                }

                TextField {
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
            UI.ui.stopReactingToDroppedConnection()
            UI.ui.disconnect()
        }
    }
}