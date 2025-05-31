import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    // Wrap the loader in an item to avoid providing inconsistent implicitHeight in the hidden state

    implicitWidth: 720
    implicitHeight: 192 - 32
    width: implicitWidth
    height: implicitHeight

    readonly property color defaultMaterialAccent: Material.accent

    Loader {
        id: loader
        anchors.fill: parent
        active: Hud.ui.isShowingChatPopup || Hud.ui.isShowingTeamChatPopup
        sourceComponent: chatComponent
    }

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: {
            if (loader.item) {
                Hud.ui.supplyDisplayedHudItemAndMargin(loader.item, 64.0)
            }
        }
    }

    Component {
        id: chatComponent
        Item {
            id: contentFrame
            width: root.width
            height: root.height
            layer.enabled: true
            layer.effect: ElevationEffect { elevation: Hud.elementElevation }

            Component.onCompleted: Hud.ui.playForwardSound()
            Component.onDestruction: Hud.destroyLayer(layer)

            Rectangle {
                anchors.fill: parent
                color: Hud.ui.isShowingTeamChatPopup ? Material.accent : "black"
                radius: 5
                opacity: Hud.elementBackgroundColor.a
            }

            TextField {
                id: input
                Material.accent: Hud.ui.isShowingTeamChatPopup ? "white" : defaultMaterialAccent

                anchors.top: parent.top
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.topMargin: 8
                anchors.leftMargin: 12
                anchors.rightMargin: 12

                font.weight: Font.Medium
                font.pointSize: Hud.labelFontSize
                font.letterSpacing: Hud.labelLetterSpacing

                Component.onCompleted: forceActiveFocus()

                onEditingFinished: {
                    if (Hud.ui.isShowingTeamChatPopup) {
                        Hud.teamChatProxy.sendMessage(text)
                    } else {
                        Hud.chatProxy.sendMessage(text)
                    }
                    text = ""
                }
            }

            ListView {
                model: Hud.ui.isShowingTeamChatPopup ? Hud.teamChatProxy.getCompactModel() : Hud.chatProxy.getCompactModel()
                verticalLayoutDirection: ListView.TopToBottom
                spacing: 4
                clip: true
                displayMarginBeginning: 0
                displayMarginEnd: 0
                anchors.top: input.bottom
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: parent.bottom
                anchors.topMargin: 0
                anchors.bottomMargin: 0
                anchors.leftMargin: 12
                anchors.rightMargin: 12

                delegate: RowLayout {
                    width: parent.width
                    HudLabel {
                        Layout.alignment: Qt.AlignTop
                        Layout.rightMargin: 2
                        font.weight: Font.Black
                        textFormat: Text.StyledText
                        style: Text.Raised
                        text: model.name + ':'
                    }
                    HudLabel {
                        Layout.fillWidth: true
                        wrapMode: Text.WordWrap
                        font.weight: Font.Medium
                        textFormat: Text.StyledText
                        clip: true
                        text: model.text
                    }
                }
            }
        }
    }

    Keys.onPressed: {
        if (Hud.ui.isShowingChatPopup || Hud.ui.isShowingTeamChatPopup) {
            if (event.key == Qt.Key_Escape || event.key == Qt.Key_Back) {
                Hud.ui.playBackSound()
                Hud.ui.closeChatPopup()
            }
        }
    }
}