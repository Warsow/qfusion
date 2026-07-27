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
    property bool clickableIfChecked: true
    property alias text: label.text
    property alias textColor: label.color

    UIFlashLabel {
        id: label
        anchors.fill: parent
        horizontalAlignment: Qt.AlignHCenter
        verticalAlignment: Qt.AlignVCenter
        color: (checked || mouseArea.containsMouse) ? Material.accent : Material.foreground
        font.capitalization: Font.AllUppercase
        font.weight: Font.Black
    }

    MouseArea {
        id: mouseArea
        hoverEnabled: true
        anchors.fill: parent
        onClicked: {
            if (!checked || clickableIfChecked) {
                label.flash()
                root.clicked()
            }
        }
        onContainsMouseChanged: {
            if (!checked || clickableIfChecked) {
                if (containsMouse) {
                    UI.ui.playHoverSound()
                    label.enter()
                } else {
                    label.leave()
                }
            } else if (!clickableIfChecked) {
                // Reset the hovered state for non-clickableIfChecked labels which become checked
                // TODO: Figure out something less hacky?
                if (!containsMouse && label._hovered) {
                    label.leave()
                }
            }
        }
    }
}
