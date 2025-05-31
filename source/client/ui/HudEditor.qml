import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Shapes 1.12
import QtQml 2.12
import net.warsow 2.6

Item {
    id: root
    clip: false

    property var hudEditorModel

    readonly property real fieldWidth: field.width
    readonly property real fieldHeight: field.height

    Item {
        id: field
        clip: false
        // Let it have an effectively fixed size, assuming that the width of the central region is fixed too
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        height: 420
        Rectangle {
            anchors.left: parent.left
            anchors.right: parent.horizontalCenter
            anchors.top: parent.top
            anchors.bottom: parent.verticalCenter
            color: Qt.rgba(1.0, 1.0, 1.0, 0.03)
        }
        Rectangle {
            anchors.left: parent.horizontalCenter
            anchors.right: parent.right
            anchors.top: parent.top
            anchors.bottom: parent.verticalCenter
            color: Qt.rgba(1.0, 1.0, 1.0, 0.05)
        }
        Rectangle {
            anchors.left: parent.left
            anchors.right: parent.horizontalCenter
            anchors.top: parent.verticalCenter
            anchors.bottom: parent.bottom
            color: Qt.rgba(1.0, 1.0, 1.0, 0.05)
        }
        Rectangle {
            anchors.left: parent.horizontalCenter
            anchors.right: parent.right
            anchors.top: parent.verticalCenter
            anchors.bottom: parent.bottom
            color: Qt.rgba(1.0, 1.0, 1.0, 0.03)
        }
        HudEditorAnchorsMarker {
            displayedAnchors: hudEditorModel.displayedFieldAnchors
        }
    }

    Repeater {
        id: itemsRepeater
        model: hudEditorModel.getLayoutModel()

        property int numInstantiatedItems: 0
        readonly property bool canArrangeItems: numInstantiatedItems === count

        delegate: HudLayoutItem {
            id: element
            width: size.width
            height: size.height

            onXChanged: handleCoordChanges()
            onYChanged: handleCoordChanges()
            Component.onCompleted: itemsRepeater.numInstantiatedItems++;
            Component.onDestruction: itemsRepeater.numInstantiatedItems--;

            property color actualColor: mouseArea.containsMouse ? Qt.lighter(model.color, 1.5) : model.color
            Behavior on actualColor { ColorAnimation { duration: 33 } }

            Rectangle {
                anchors.fill: parent
                color: Qt.rgba(actualColor.r, actualColor.g, actualColor.b, 0.3)
                border.color: Qt.rgba(actualColor.r, actualColor.g, actualColor.b, 0.7)
                border.width: 2
                layer.enabled: true
                layer.effect: ElevationEffect { elevation: 4 }
            }

            Label {
                visible: model.displayedAnchors !== (HudLayoutModel.VCenter | HudLayoutModel.HCenter)
                anchors.centerIn: parent
                width: parent.width - 16
                horizontalAlignment: Qt.AlignHCenter
                verticalAlignment: Qt.AlignVCenter
                maximumLineCount: 2
                wrapMode: Text.WordWrap
                font.pointSize: 10.5
                font.letterSpacing: 0.5
                font.capitalization: Font.AllUppercase
                text: name
                font.weight: Font.Medium
            }

            states: [
                State {
                    name: "anchored"
                    when: !mouseArea.containsMouse && itemsRepeater.canArrangeItems
                    AnchorChanges {
                        target: element
                        anchors.top: getQmlAnchor(HudLayoutModel.Top)
                        anchors.bottom: getQmlAnchor(HudLayoutModel.Bottom)
                        anchors.left: getQmlAnchor(HudLayoutModel.Left)
                        anchors.right: getQmlAnchor(HudLayoutModel.Right)
                        anchors.horizontalCenter: getQmlAnchor(HudLayoutModel.HCenter)
                        anchors.verticalCenter: getQmlAnchor(HudLayoutModel.VCenter)
                    }
                },
                State {
                    name: "dragged"
                    when: mouseArea.containsMouse && mouseArea.drag.active && itemsRepeater.canArrangeItems
                    AnchorChanges {
                        target: element
                        anchors.top: undefined
                        anchors.bottom: undefined
                        anchors.left: undefined
                        anchors.right: undefined
                        anchors.horizontalCenter: undefined
                        anchors.verticalCenter: undefined
                    }
                    PropertyChanges {
                        target: element
                        x: origin.x
                        y: origin.y
                    }
                },
                State {
                    name: "detached"
                    when: mouseArea.containsMouse && !mouseArea.drag.active && itemsRepeater.canArrangeItems
                    AnchorChanges {
                        target: element
                        anchors.top: undefined
                        anchors.bottom: undefined
                        anchors.left: undefined
                        anchors.right: undefined
                        anchors.horizontalCenter: undefined
                        anchors.verticalCenter: undefined
                    }
                }
            ]

            function getQmlAnchor(anchorBit) {
                let anchorItem
                if (anchorItemIndex > 0) {
                    anchorItem = itemsRepeater.itemAt(anchorItemIndex - 1)
                } else if (anchorItemIndex < 0) {
                    anchorItem = field
                } else {
                    // We can not longer just address by kind for mini models, also that was fragile
                    for (let i = 0; i < toolboxRepeater.count; ++i) {
                        let item = toolboxRepeater.itemAt(i)
                        if (item.kind == kind) {
                            anchorItem = item
                            break
                        }
                    }
                    console.assert(anchorItem)
                }
                return getQmlAnchorOfItem(selfAnchors, anchorItemAnchors, anchorBit, anchorItem)
            }

            function handleCoordChanges() {
                if (itemsRepeater.canArrangeItems) {
                    if (mouseArea.drag.active) {
                        hudEditorModel.trackDragging(index, element.x, element.y)
                    } else {
                        hudEditorModel.updateElementPosition(index, element.x, element.y)
                    }
                }
            }

            HudEditorAnchorsMarker {
                displayedAnchors: model.displayedAnchors
                highlighted: mouseArea.drag.active
            }

            MouseArea {
                id: mouseArea
                anchors.fill: parent
                drag.target: draggable ? parent : undefined
                hoverEnabled: false
                drag.onActiveChanged: {
                    if (!drag.active) {
                        hudEditorModel.finishDragging(index)
                    }
                }
                onContainsMouseChanged: {
                    if (!containsMouse) {
                        hudEditorModel.clearDisplayedMarkers(index)
                    }
                }
            }
        }
    }

    readonly property real toolboxXSpacing: 16
    readonly property real toolboxYSpacing: 20
    readonly property real toolboxInitialX: 32
    readonly property real toolboxInitialY: field.height + 2.0 * toolboxYSpacing

    property real toolboxNextX: toolboxInitialX
    property real toolboxNextY: toolboxInitialY
    property real toolboxRowHeightSoFar: 0
    property real toolboxMaxXSoFar: 0.0

    Rectangle {
        anchors.top: parent.top
        anchors.topMargin: toolboxInitialY - 16
        anchors.bottom: parent.bottom
        anchors.bottomMargin: -16
        anchors.left: parent.left
        anchors.right: parent.right
        color: Qt.rgba(0.0, 0.0, 0.0, 0.1)
    }

    // Toolbox items must be siblings of hud items so they are instantiated within the same parent
    Repeater {
        id: toolboxRepeater
        model: hudEditorModel.getToolboxModel()
        delegate: Item {
            id: toolboxItem
            width: model.size.width
            height: model.size.height
            property int kind: model.kind
            Shape {
                anchors.fill: parent
                ShapePath {
                    fillColor: "transparent"
                    strokeWidth: 2
                    // Choose a nicer formatting at cost of some redundancy
                    strokeColor: model.displayedAnchors ?
                        Qt.lighter(Qt.rgba(model.color.r, model.color.g, model.color.b, 1.0), 1.25) :
                        Qt.lighter(Qt.rgba(model.color.r, model.color.g, model.color.b, 0.3), 1.25)
                    Behavior on strokeColor { ColorAnimation { duration: 33 } }
                    strokeStyle: ShapePath.DashLine
                    joinStyle: ShapePath.MiterJoin
                    startX: 0; startY: 0
                    PathLine { x: toolboxItem.width; y: 0 }
                    PathLine { x: toolboxItem.width; y: toolboxItem.height }
                    PathLine { x: 0; y: toolboxItem.height }
                    PathLine { x: 0; y: 0 }
                }
            }
            HudEditorAnchorsMarker {
                displayedAnchors: model.displayedAnchors
                highlighted: false
            }
            Component.onCompleted: {
                toolboxItem.parent = root
                // Account for repeated instantiation/initialization of items that happens (wtf?)
                if (index == 0) {
                    toolboxNextX = toolboxInitialX
                    toolboxNextY = toolboxInitialY
                    toolboxRowHeightSoFar = 0.0
                } else if (toolboxNextX + toolboxItem.width + toolboxXSpacing > root.width) {
                    toolboxNextX = toolboxInitialX
                    toolboxNextY += toolboxRowHeightSoFar + toolboxYSpacing
                    toolboxRowHeightSoFar = 0.0
                }
                toolboxItem.x = toolboxNextX
                toolboxItem.y = toolboxNextY
                toolboxNextX += toolboxItem.width + toolboxXSpacing
                toolboxRowHeightSoFar = Math.max(toolboxItem.height, toolboxRowHeightSoFar)
                toolboxMaxXSoFar = Math.max(toolboxNextX, toolboxMaxXSoFar)
                hudEditorModel.updatePlaceholderPosition(index, toolboxItem.x, toolboxItem.y)
            }
        }
    }
}