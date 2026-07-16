import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

MouseArea {
    id: root
    height: 36

    hoverEnabled: true

    property bool selected
    property bool detailed
    property string text

    onContainsMouseChanged: {
        if (containsMouse) {
            UI.ui.playHoverSound()
            label.enter()
        } else {
            label.leave()
        }
    }

    onClicked: label.flash()

    UIFlashLabel {
        id: label
        text: root.text
        anchors.right: undefined
        anchors.horizontalCenter: parent.horizontalCenter

        color: (root.containsMouse || root.selected) ? Material.accent : Material.foreground
        font.weight: Font.Bold
        font.capitalization: Font.AllUppercase

        transitions: Transition {
            AnchorAnimation {
                duration: 250
            }
        }

        state: "initial"
        states: [
            State {
                name: "detailed"
                when: detailed
                AnchorChanges {
                    target: label
                    anchors.right: root.right
                    anchors.horizontalCenter: undefined
                }
                PropertyChanges {
                    target: label
                    flashAlignment: Qt.AlignRight
                }
            },
            State {
                name: "initial"
                when: !detailed
                AnchorChanges {
                    target: label
                    anchors.right: undefined
                    anchors.horizontalCenter: root.horizontalCenter
                }
                PropertyChanges {
                    target: label
                    flashAlignment: Qt.AlignHCenter
                }
            }
        ]
    }
}