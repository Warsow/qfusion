import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import net.warsow 2.6

Item {
    id: root
    implicitHeight: parent.height
    clip: false

    signal bindingRequested(int quakeKey)
    signal unbindingRequested(int quakeKey)
    signal keySelected(int quakeKey)

    property string text: ""
    property int quakeKey: -1
    property int rowSpan: 1
    property int group: 0
    property real rowSpacing
    property bool hidden: false

    property bool isInEditorMode
    property bool externallyHighlighted
    property bool highlighted: isInEditorMode ?
        (mouseArea.containsMouse && !group) :
        (mouseArea.containsMouse || externallyHighlighted)

    property bool isASpecialGlyph: (root.text.length === 1 && root.text.charCodeAt(0) > 127)
    property real fontPointSize: UI.labelFontSize

    readonly property color highlightColor: UI.keysAndBindings.colorForGroup(root.group)
    readonly property color highlightBackground: Qt.rgba(highlightColor.r, highlightColor.g, highlightColor.b, 0.075)

    readonly property bool isActionAvailable: mouseArea.containsMouse && mouseArea.mouseX < 16 && !isInEditorMode

    Connections {
        target: UI.keysAndBindings
        onKeyExternalHighlightChanged: {
            if (root.quakeKey == targetQuakeKey) {
                externallyHighlighted = targetHighlighted
            }
        }
    }

    Rectangle {
        color: !hidden ? (root.group ? highlightBackground : Qt.rgba(0, 0, 0, 0.3)) : "transparent"
        radius: 5
        width: parent.width
        height: parent.height * rowSpan + rowSpacing * (rowSpan - 1)

        border {
            width: highlighted ? 2 : 0
            color: root.group ? highlightColor : Material.accentColor
        }
        anchors {
            top: parent.top
            left: parent.left
        }

        Rectangle {
            anchors {
                left: parent.left
                leftMargin: 5
                verticalCenter: parent.verticalCenter
            }
            width: 5
            height: 5
            radius: 2.5
            visible: !!root.group && root.visible && root.enabled && !isActionAvailable
            color: highlightColor
        }

        Label {
            visible: isActionAvailable
            anchors.left: parent.left
            anchors.leftMargin: 5
            anchors.verticalCenter: parent.verticalCenter
            anchors.verticalCenterOffset: 2
            font.family: UI.ui.symbolsFontFamily
            font.pointSize: 13
            text: group ? String.fromCodePoint(0x1F7A9) : String.fromCodePoint(0x1F7A2)
        }

        MouseArea {
            id: mouseArea
            enabled: !root.hidden && root.enabled
            hoverEnabled: true
            anchors.centerIn: parent
            width: parent.width - 10
            height: parent.height - 10
            onContainsMouseChanged: {
                if (containsMouse) {
                    UI.ui.playHoverSound()
                }
                if (!isInEditorMode) {
                    UI.keysAndBindings.onKeyItemContainsMouseChanged(quakeKey, mouseArea.containsMouse)
                }
            }
            onClicked: {
                if (isInEditorMode) {
                    UI.ui.playForwardSound()
                    keySelected(quakeKey)
                } else if (group) {
                    UI.ui.playSwitchSound()
                    unbindingRequested(quakeKey)
                } else {
                    UI.ui.playForwardSound()
                    bindingRequested(quakeKey)
                }
            }
        }

        Label {
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.verticalCenter: parent.verticalCenter
            anchors.verticalCenterOffset: root.isASpecialGlyph ? +2 : 0
            text: !root.hidden ? root.text : ""
            font.family: root.isASpecialGlyph ? UI.ui.symbolsFontFamily : UI.ui.regularFontFamily
            font.pointSize: root.fontPointSize
            font.letterSpacing: UI.labelLetterSpacing
            font.weight: Font.Medium
            color: Material.foreground
        }
    }
}