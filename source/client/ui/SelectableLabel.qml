import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root

    property string text
    property bool selected

    property int horizontalAlignment: Qt.AlignHCenter
    property int verticalAlignment: Qt.AlignHCenter

    implicitWidth: label.implicitWidth
    implicitHeight: label.implicitHeight

    signal clicked()

    MouseArea {
        id: mouseArea
        enabled: root.enabled
        hoverEnabled: true
        anchors.fill: label
        onClicked: {
            label.flash()
            root.clicked()
        }
        onContainsMouseChanged: {
            if (containsMouse) {
                UI.ui.playHoverSound()
                label.enter()
            } else {
                label.leave()
            }
        }
    }

    UIFlashLabel {
        id: label
        width: root.width
        height: implicitHeight
        horizontalAlignment: root.horizontalAlignment
        verticalAlignment: root.verticalAlignment
        font.weight: Font.ExtraBold
        font.capitalization: Font.AllUppercase
        color: mouseArea.containsMouse || selected ? Material.accent : Material.foreground
        opacity: root.enabled ? 1.0 : 0.5
        style: root.enabled ? Text.Raised : Text.Normal
        text: root.text
    }
}