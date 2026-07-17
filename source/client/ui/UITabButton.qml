import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root
    width: implicitWidth
    height: implicitHeight

    implicitWidth: label.implicitWidth
    implicitHeight: UI.tabHeight

    signal clicked()

    property bool checked
    property alias text: label.text
    property alias textColor: label.color

    UILabel {
        id: label
        anchors.fill: parent
        horizontalAlignment: Qt.AlignHCenter
        verticalAlignment: Qt.AlignVCenter
        color: (checked || mouseArea.containsMouse) ? Material.accent : Material.foreground
        font.pointSize: (checked || mouseArea.containsMouse) ? UI.labelFontSize + 2 : UI.labelFontSize
        font.letterSpacing: checked ? 2.0 : 1.25
        font.capitalization: Font.AllUppercase
        font.weight: Font.Black
        Behavior on font.pointSize { SmoothedAnimation { duration: 250 } }
        Behavior on font.letterSpacing { SmoothedAnimation { duration: 250 } }
    }

    MouseArea {
        id: mouseArea
        hoverEnabled: true
        anchors.fill: parent
        onClicked: root.clicked()
        onContainsMouseChanged: {
            if (containsMouse) {
                UI.ui.playHoverSound()
            }
        }
    }
}
