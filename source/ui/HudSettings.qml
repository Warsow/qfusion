import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    Item {
        id: field
        anchors.centerIn: parent
        width: 16 * 50
        height: 9 * 50
        clip: true

        Component.onCompleted: hudEditorLayoutModel.setFieldSize(width, height)

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

        Repeater {
            id: repeater
            model: hudEditorLayoutModel
            delegate: Rectangle {
                id: element
                width: size.width
                height: size.height
                color: Qt.rgba(1.0, 0.0, 0.3, 0.3)
                onXChanged: handleCoordChanges()
                onYChanged: handleCoordChanges()
                state: "anchored"

                states: [
                    State {
                        name: "anchored"
                        when: !mouseArea.containsMouse
                        AnchorChanges {
                            target: element
                            anchors.bottom: getQmlAnchor(HudLayoutModel.Bottom)
                            anchors.top: getQmlAnchor(HudLayoutModel.Top)
                            anchors.left: getQmlAnchor(HudLayoutModel.Left)
                            anchors.right: getQmlAnchor(HudLayoutModel.Right)
                            anchors.horizontalCenter: getQmlAnchor(HudLayoutModel.HCenter)
                            anchors.verticalCenter: getQmlAnchor(HudLayoutModel.VCenter)
                        }
                    },
                    State {
                        name: "dragged"
                        when: mouseArea.containsMouse && mouseArea.drag.active
                        AnchorChanges {
                            target: element
                            anchors.bottom: undefined
                            anchors.horizontalCenter: undefined
                            anchors.left: undefined
                            anchors.right: undefined
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
                        when: mouseArea.containsMouse && !mouseArea.drag.active
                        AnchorChanges {
                            target: element
                            anchors.baseline: undefined
                            anchors.horizontalCenter: undefined
                            anchors.left: undefined
                            anchors.right: undefined
                            anchors.verticalCenter: undefined
                        }
                    }
                ]

                // Translates natively supplied (numeric) flags to an AnchorLine of another item.
                // Assumes that flags consist of two parts each of that has a single bit set.
                // This means that a HUD item position is described by 2 of 6 supported anchors
                // (leaving the unused anchors.baseline out of scope).
                function getQmlAnchor(anchorBit) {
                    // If this anchorBit is specfied for self anchors
                    if (selfAnchors & anchorBit) {
                        const anchorItem = anchorItemIndex >= 0 ? repeater.itemAt(anchorItemIndex) : field
                        const horizontalMask = HudLayoutModel.Left | HudLayoutModel.HCenter | HudLayoutModel.Right
                        // If this anchor bit describes a horizontal rule
                        if (anchorBit & horizontalMask) {
                            // Check what horizontal anchor of other item is specified
                            switch (anchorItemAnchors & horizontalMask) {
                                case HudLayoutModel.Left: return anchorItem.left
                                case HudLayoutModel.HCenter: return anchorItem.horizontalCenter
                                case HudLayoutModel.Right: return anchorItem.right
                            }
                            console.log("unreachable")
                        }
                        // If this anchor bit describes a vertial rule
                        const verticalMask = HudLayoutModel.Top | HudLayoutModel.VCenter | HudLayoutModel.Bottom
                        if (anchorBit & verticalMask) {
                            // Check what vertical anchor of other item is specified
                            switch (anchorItemAnchors & verticalMask) {
                                case HudLayoutModel.Top: return anchorItem.top
                                case HudLayoutModel.VCenter: return anchorItem.verticalCenter
                                case HudLayoutModel.Bottom: return anchorItem.bottom
                            }
                            console.log("unreachable")
                        }
                    }
                    return undefined
                }

                function handleCoordChanges() {
                    if (mouseArea.drag.active) {
                        hudEditorLayoutModel.trackDragging(index, element.x, element.y)
                    } else {
                        hudEditorLayoutModel.updatePosition(index, element.x, element.y)
                    }
                }

                Rectangle {
                    visible: displayedAnchors
                    color: mouseArea.drag.active ? Material.accent : "pink"
                    width: 4; height: 4
                    anchors.centerIn: parent
                    anchors.horizontalCenterOffset:
                        displayedAnchors & HudLayoutModel.Left ?
                            (-parent.width / 2) :
                            (displayedAnchors & HudLayoutModel.Right ? parent.width / 2 : 0)
                    anchors.verticalCenterOffset:
                        displayedAnchors & HudLayoutModel.Top ?
                            (-parent.height / 2) :
                            (displayedAnchors & HudLayoutModel.Bottom ? parent.height / 2 : 0)
                }

                MouseArea {
                    id: mouseArea
                    anchors.fill: parent
                    drag.target: parent
                    hoverEnabled: true
                    drag.onActiveChanged: {
                        if (!drag.active) {
                            hudEditorLayoutModel.finishDragging(index)
                        }
                    }
                    onContainsMouseChanged: {
                        if (!containsMouse) {
                            hudEditorLayoutModel.updateAnchors(index)
                        }
                    }
                }
            }
        }
    }
}

