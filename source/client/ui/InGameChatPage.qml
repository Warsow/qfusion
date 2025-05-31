import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    property bool isDisplayingTeamChat: false

    RowLayout {
        id: header
        width: parent.width - 16
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter

        Item {
            Layout.fillWidth: true
        }

        Button {
            flat: true
            visible: UI.ui.hasTeamChat
            Material.theme: Material.Dark
            text: isDisplayingTeamChat ? "switch to common" : "switch to team"
            onHoveredChanged: {
                if (hovered) {
                    UI.ui.playHoverSound()
                }
            }
            onClicked: {
                UI.ui.playSwitchSound()
                isDisplayingTeamChat = !isDisplayingTeamChat
            }
        }

        // Unused for now
    }

    Connections {
        target: UI.ui
        onHasTeamChatChanged: {
            isDisplayingTeamChat = false
        }
    }

    RichChatList {
        id: chatList
        model: isDisplayingTeamChat ? UI.teamChatProxy.getRichModel() : UI.chatProxy.getRichModel()
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: header.bottom
        anchors.bottom: inputFrame.top
        // It look slightly lesser due to the content alginment
        anchors.leftMargin: 12
        anchors.rightMargin: 12
        anchors.topMargin: 8
        anchors.bottomMargin: 12
        clip: true
        onCountChanged: positionViewAtBeginning()
        onModelChanged: positionViewAtBeginning()
    }

    Rectangle {
        anchors.horizontalCenter: inputFrame.horizontalCenter
        anchors.verticalCenter: inputFrame.verticalCenter
        color: Qt.lighter(Material.background, 1.25)
        opacity: 0.5
        width: inputFrame.width + 12
        height: inputFrame.height + 4
        radius: 3
    }

    ScrollView {
        id: inputFrame
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.margins: 20
        leftPadding: 4
        rightPadding: 4
        height: Math.min(Math.max(72, input.implicitHeight), 128)

        TextArea {
            id: input
            width: parent.width
            selectByMouse: false
            selectByKeyboard: false
            wrapMode: TextEdit.Wrap
            font.letterSpacing: UI.labelLetterSpacing
            font.pointSize: UI.labelFontSize
            Material.theme: (activeFocus || text.length > 0) ? Material.Light : Material.Dark
            placeholderText: activeFocus ? "" : "\u2026"

            onTextChanged: {
                // TODO: Count bytes/respect native code limitations on the number of bytes
                if (length > 200) {
                    remove(200, length)
                }
            }

            Keys.onPressed: {
                if (event.key === Qt.Key_Enter) {
                    if (isDisplayingTeamChat) {
                        UI.teamChatProxy.sendMessage(text)
                    } else {
                        UI.chatProxy.sendMessage(text)
                    }
                    // We should clear the key that is being entered now, defer to the next frame
                    clearOnNextFrameTimer.start()
                }
            }

            Timer {
                id: clearOnNextFrameTimer
                interval: 1
                onTriggered: input.clear()
            }
        }
    }
}