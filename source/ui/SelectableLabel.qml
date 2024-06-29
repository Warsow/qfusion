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
        anchors.centerIn: parent
        width: label.implicitWidth
        height: label.implicitHeight
        onClicked: root.clicked()
        onContainsMouseChanged: {
            if (containsMouse) {
                UI.ui.playHoverSound()
            }
        }
    }

    UILabel {
        id: label
        width: root.width
        height: implicitHeight
        horizontalAlignment: root.horizontalAlignment
        verticalAlignment: root.verticalAlignment
        font.weight: Font.ExtraBold
        font.capitalization: Font.AllUppercase
        font.letterSpacing: mouseArea.containsMouse ? 2.5 : 1.5
        Behavior on font.letterSpacing { SmoothedAnimation { duration: 250 } }
        color: mouseArea.containsMouse || selected ? Material.accent : Material.foreground
        opacity: root.enabled ? 1.0 : 0.5
        text: root.text
    }
}