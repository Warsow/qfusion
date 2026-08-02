import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

FocusScope {
    id: root
    property bool isDisplayingTeamChat: false

    StackView.onStatusChanged: {
        if (StackView.status === StackView.Deactivating) {
            if (isDisplayingTeamChat) {
                teamAppearDisappearHelper.shrinkAndHide()
            } else {
                commonAppearDisappearHelper.shrinkAndHide()
            }
            input.clear()
        }
    }

    AppearDisappearHelper {
        id: commonAppearDisappearHelper
        targets: [commonHeader, commonButton]
    }

    AppearDisappearHelper {
        id: teamAppearDisappearHelper
        targets: [teamHeader, teamButton]
        appearDelay: -1
    }

    UIHeaderLabel {
        id: commonHeader
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter
        baseText: "Common chat"
    }
    UIHeaderLabel {
        id: teamHeader
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter
        baseText: "Team chat"
    }

    UITabButton {
        id: commonButton
        anchors.right: parent.right
        text: "Team"
        visible: UI.ui.hasTeamChat
        enabled: !isDisplayingTeamChat
        onClicked: {
            UI.ui.playSwitchSound()
            isDisplayingTeamChat = true
        }
    }
    UITabButton {
        id: teamButton
        anchors.right: parent.right
        text: "Common"
        visible: UI.ui.hasTeamChat
        enabled: isDisplayingTeamChat
        onClicked: {
            UI.ui.playSwitchSound()
            isDisplayingTeamChat = false
        }
    }

    onIsDisplayingTeamChatChanged: {
        if (isDisplayingTeamChat) {
            commonAppearDisappearHelper.expandAndHide()
            teamAppearDisappearHelper.show()
        } else {
            teamAppearDisappearHelper.expandAndHide()
            commonAppearDisappearHelper.show()
        }
    }

    Connections {
        target: UI.ui
        onHasTeamChatChanged: {
            // Note: We don't animate availabilty of switch chat buttons
            if (isDisplayingTeamChat) {
                isDisplayingTeamChat = false
            }
        }
    }

    RichChatList {
        id: chatList
        model: isDisplayingTeamChat ? UI.teamChatProxy.getRichModel() : UI.chatProxy.getRichModel()
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.bottom: input.top
        anchors.topMargin: 8 + UI.tabHeight
        anchors.bottomMargin: 20
        anchors.leftMargin: 12
        anchors.rightMargin: 12
        clip: true
        onCountChanged: positionViewAtBeginning()
        onModelChanged: positionViewAtBeginning()
        UILabel {
            visible: chatList.count === 0
            anchors.centerIn: parent
            text: "No messages"
        }
    }

    Rectangle {
        anchors.horizontalCenter: input.horizontalCenter
        anchors.verticalCenter: input.verticalCenter
        color: Qt.lighter(Material.background, 1.5)
        opacity: 0.7
        width: root.width
        height: input.height + 8
        radius: 6
        layer.enabled: true
        layer.effect: ElevationEffect { elevation: 16 }
    }

    TextArea {
        id: input
        focus: true
        anchors.left: parent.left
        anchors.right: parent.right
        height: Math.max(72, implicitHeight)
        anchors.bottom: parent.bottom
        anchors.leftMargin: 12
        anchors.rightMargin: 12
        anchors.bottomMargin: 20
        selectByMouse: false
        selectByKeyboard: false
        wrapMode: TextEdit.Wrap
        font.letterSpacing: UI.labelLetterSpacing
        font.pointSize: UI.labelFontSize
        Material.theme: (activeFocus || text.length > 0) ? Material.Light : Material.Dark
        placeholderText: activeFocus ? "" : "Type here\u2026"
        background: null
        Material.accent: "white"

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