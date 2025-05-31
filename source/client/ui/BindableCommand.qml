import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

MouseArea {
    id: root

    signal bindingRequested(int command)
    signal bindingSelected(int command)

    property string text
    property string command
    property int commandNum
    property color highlightColor

    property real defaultLabelWidth
    property real defaultLabelHeight

    property bool isBound
    property bool allowMultiBind
    property bool isInEditorMode
    property bool externallyHighlighted
    readonly property bool highlighted: isInEditorMode ?
        (containsMouse && (!isBound || allowMultiBind)) :
        (isBound && (containsMouse || externallyHighlighted))

    opacity: hoverEnabled ? 1.0 : 0.5
    hoverEnabled: !isInEditorMode || !isBound || allowMultiBind

    implicitHeight: Math.max(marker.height, defaultLabelHeight) + 4
    implicitWidth: marker.width + defaultLabelWidth + 12

    Connections {
        target: UI.keysAndBindings
        onCommandExternalHighlightChanged: {
            if (root.commandNum === targetCommandNum) {
                externallyHighlighted = targetHighlighted
            }
        }
    }

    onContainsMouseChanged: {
        if (isBound && containsMouse) {
            UI.ui.playHoverSound()
        }
        // Don't highlight
        if (!isInEditorMode) {
            UI.keysAndBindings.onCommandItemContainsMouseChanged(commandNum, containsMouse)
        }
    }

    onClicked: {
        if (isInEditorMode) {
            UI.ui.playForwardSound()
            bindingSelected(commandNum)
        } else if (bindMarker.visible) {
            UI.ui.playForwardSound()
            bindingRequested(commandNum)
        }
    }

    Rectangle {
        id: marker
        visible: isBound
        width: 5
        height: 5
        radius: 2.5
        color: highlighted ? highlightColor : Material.foreground
        anchors {
            left: parent.left
            verticalCenter: parent.verticalCenter
        }
    }

    UILabel {
        id: bindMarker
        visible: !isBound && containsMouse && mouseX < 16
        anchors {
            left: parent.left
            leftMargin: -2
            verticalCenter: parent.verticalCenter
        }
        onVisibleChanged: {
            if (visible) {
                UI.ui.playHoverSound()
            }
        }
        font.weight: Font.Medium
        text: "+"
    }

    UILabel {
        id: label
        text: root.text
        color: highlighted ? highlightColor : Material.foreground
        font.underline: highlighted
        font.weight: highlighted ? Font.Bold : Font.Normal
        anchors {
            left: marker.right
            leftMargin: 12
            verticalCenter: parent.verticalCenter
        }

        Component.onCompleted: {
            root.defaultLabelWidth = label.implicitWidth
            root.defaultLabelHeight = label.implicitHeight
        }
    }
}
