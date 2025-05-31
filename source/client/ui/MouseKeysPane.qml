import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import QtQuick.Shapes 1.12
import net.warsow 2.6

Rectangle {
    id: root
    signal bindingRequested(int quakeKey)
    signal unbindingRequested(int quakeKey)
    signal keySelected(int quakeKey)

    property bool isInEditorMode

    color: Qt.rgba(0, 0, 0, 0.2)
    radius: 6

    readonly property real referenceSide: 0.97 * mouseBody.height
    readonly property color defaultStrokeColor: Qt.rgba(1.0, 1.0, 1.0, 0.03)

    readonly property real buttonHeight: 28
    readonly property real drawnButtonWidth: 56
    readonly property real otherButtonWidth: 64

    Shape {
        id: mouseBody
        anchors.left: parent.left
        anchors.leftMargin: 62
        anchors.verticalCenter: parent.verticalCenter
        width: parent.height
        height: 0.85 * parent.height
        smooth: true
        layer.enabled: true
        layer.samples: 2

        ShapePath {
            fillColor: Qt.rgba(0, 0, 0, 0.3)
            strokeWidth: 2
            strokeColor: defaultStrokeColor
            startX: 0.5 * mouseBody.width
            startY: 3

            // Right half
            PathCurve { relativeX: +0.20 * referenceSide; relativeY: +0.05 * referenceSide }
            PathCurve { relativeX: +0.10 * referenceSide; relativeY: +0.30 * referenceSide }
            PathCurve { relativeX: +0.00 * referenceSide; relativeY: +0.45 * referenceSide }
            PathCurve { relativeX: -0.30 * referenceSide; relativeY: +0.20 * referenceSide }

            // Left half
            PathCurve { relativeX: -0.30 * referenceSide; relativeY: -0.20 * referenceSide }
            PathCurve { relativeX: +0.00 * referenceSide; relativeY: -0.45 * referenceSide }
            PathCurve { relativeX: +0.10 * referenceSide; relativeY: -0.30 * referenceSide }
            PathCurve { relativeX: +0.20 * referenceSide; relativeY: -0.05 * referenceSide }
        }
    }

    MouseButtonShape {
        anchors.right: mouseBody.horizontalCenter
        anchors.top: mouseBody.top
        width: 0.5 * mouseBody.width
        height: mouseBody.height
        isLeft: true
        referenceSide: root.referenceSide
        highlighted: leftButtonKey.highlighted
        highlightColor: leftButtonKey.highlightColor
        defaultStrokeColor: root.defaultStrokeColor
    }

    MouseButtonShape {
        anchors.left: mouseBody.horizontalCenter
        anchors.top: mouseBody.top
        width: 0.5 * mouseBody.width
        height: mouseBody.height
        isLeft: false
        referenceSide: root.referenceSide
        highlighted: rightButtonKey.highlighted
        highlightColor: rightButtonKey.highlightColor
        defaultStrokeColor: root.defaultStrokeColor
    }

    Label {
        id: scrollUpShape
        anchors.bottom: wheelShape.top
        anchors.bottomMargin: -8
        anchors.horizontalCenter: wheelShape.horizontalCenter
        font.family: UI.ui.symbolsFontFamily
        font.pointSize: 16
        text: "\u25B4"
        color: "transparent"
    }

    Label {
        id: scrollDownShape
        anchors.top: wheelShape.bottom
        anchors.topMargin: -2
        anchors.horizontalCenter: wheelShape.horizontalCenter
        font.family: UI.ui.symbolsFontFamily
        font.pointSize: 16
        text: "\u25BE"
        color: "transparent"
    }

    Label {
        id: middleButtonShape
        anchors.horizontalCenter: wheelShape.horizontalCenter
        anchors.verticalCenter: wheelShape.verticalCenter
        anchors.verticalCenterOffset: +3
        font.family: UI.ui.symbolsFontFamily
        font.pointSize: 16
        text: "\u25AA"
        color: "transparent"
    }

    Rectangle {
        id: wheelShape
        property bool highlighted
        property color highlightColor
        anchors.top: mouseBody.top
        anchors.topMargin: 0.15 * referenceSide
        anchors.horizontalCenter: mouseBody.horizontalCenter
        width: 10
        height: 36
        color: "transparent"
        radius: 5
        border.width: 2
        border.color: highlighted ?
            (highlightColor != Qt.rgba(0, 0, 0, 0) ? highlightColor : Material.accent) : defaultStrokeColor
    }

    MouseKey {
        id: leftButtonKey
        text: "L"
        isInEditorMode: root.isInEditorMode
        quakeKey: UI.keysAndBindings.getMouseButtonKeyCode(1)
        width: drawnButtonWidth
        height: buttonHeight
        anchors.right: mouseBody.left
        anchors.top: mouseBody.top
        onBindingRequested: root.bindingRequested(quakeKey)
        onUnbindingRequested: root.unbindingRequested(quakeKey)
        onKeySelected: root.keySelected(quakeKey)
    }

    MouseKey {
        id: rightButtonKey
        text: "R"
        isInEditorMode: root.isInEditorMode
        quakeKey: UI.keysAndBindings.getMouseButtonKeyCode(2)
        width: drawnButtonWidth
        height: buttonHeight
        anchors.left: mouseBody.right
        anchors.top: mouseBody.top
        onBindingRequested: root.bindingRequested(quakeKey)
        onUnbindingRequested: root.unbindingRequested(quakeKey)
        onKeySelected: root.keySelected(quakeKey)
    }

    readonly property var wheelKeysModel: [
        { text: "\u25B4", quakeKey: UI.keysAndBindings.getMouseWheelKeyCode(true), shape: scrollUpShape },
        { text: "\u25AA", quakeKey: UI.keysAndBindings.getMouseButtonKeyCode(3), shape: middleButtonShape },
        { text: "\u25BE", quakeKey: UI.keysAndBindings.getMouseWheelKeyCode(false), shape: scrollDownShape }
    ]

    ColumnLayout {
        anchors.right: mouseBody.left
        anchors.verticalCenter: parent.verticalCenter
        anchors.verticalCenterOffset: 20
        spacing: 4
        Repeater {
            model: root.wheelKeysModel
            MouseKey {
                text: modelData["text"]
                quakeKey: modelData["quakeKey"]
                isASpecialGlyph: true
                fontPointSize: 16
                isInEditorMode: root.isInEditorMode
                Layout.preferredWidth: drawnButtonWidth
                Layout.preferredHeight: buttonHeight
                onHighlightedChanged: {
                    wheelShape.highlighted = highlighted
                    wheelShape.highlightColor = highlightColor
                    // This is not actually a shape but any item with an RGBA "color" property
                    let contextualShape = modelData["shape"]
                    if (highlighted) {
                        contextualShape.color = highlightColor != Qt.rgba(0, 0, 0, 0) ? highlightColor : Material.accent
                    } else {
                        contextualShape.color = "transparent"
                    }
                }
                onBindingRequested: root.bindingRequested(quakeKey)
                onUnbindingRequested: root.unbindingRequested(quakeKey)
                onKeySelected: root.keySelected(quakeKey)
            }
        }
    }

    ColumnLayout {
        id: leftSideButtons
        anchors.right: rightSideButtons.left
        anchors.rightMargin: 8
        anchors.bottom: rightSideButtons.bottom
        spacing: 4
        Repeater {
            model: 2
            MouseKey {
                text: "#" + (index + 4)
                isInEditorMode: root.isInEditorMode
                quakeKey: UI.keysAndBindings.getMouseButtonKeyCode(index + 4)
                Layout.preferredWidth: otherButtonWidth
                Layout.preferredHeight: buttonHeight
                onBindingRequested: root.bindingRequested(quakeKey)
                onUnbindingRequested: root.unbindingRequested(quakeKey)
                onKeySelected: root.keySelected(quakeKey)
            }
        }
    }

    ColumnLayout {
        id: rightSideButtons
        anchors.right: parent.right
        anchors.rightMargin: 8
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 8
        spacing: 4
        Repeater {
            model: 3
            MouseKey {
                text: "#" + (index + 6)
                isInEditorMode: root.isInEditorMode
                quakeKey: UI.keysAndBindings.getMouseButtonKeyCode(index + 6)
                Layout.preferredWidth: otherButtonWidth
                Layout.preferredHeight: buttonHeight
                onBindingRequested: root.bindingRequested(quakeKey)
                onUnbindingRequested: root.unbindingRequested(quakeKey)
                onKeySelected: root.keySelected(quakeKey)
            }
        }
    }
}