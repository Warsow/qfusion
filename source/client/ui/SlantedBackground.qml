import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Shapes 1.12
import QtGraphicalEffects 1.12
import net.warsow 2.6

Item {
    id: root

    property real radius: 0.0
    property real leftPartSkewDegrees: 0.0
    property real rightPartSkewDegrees: 0.0
    property color fillColor
    property color outlineColor: fillColor
    property real shadowOpacity: 1.0
    property bool enabled: true

    Loader {
        id: loader
        anchors.centerIn: parent
        width: item ? item.width : 0
        height: parent.height
        sourceComponent:
            root.leftPartSkewDegrees != root.rightPartSkewDegrees || root.fillColor !== root.outlineColor ?
                complexComponent : simpleComponent
    }

    Component {
        id: simpleComponent
        Item {
            readonly property real halfShift: 0.5 * Math.tan(leftPartSkewDegrees * Math.PI / 180.0) * root.height
            readonly property var transformMatrix: UI.ui.makeSkewXMatrix(root.height, leftPartSkewDegrees)
                .times(UI.ui.makeTranslateMatrix(-halfShift, 0.0))

            width: root.width + 2.0 * (Math.abs(halfShift) + root.radius + 1.0)
            height: root.height

            Rectangle {
                anchors.centerIn: parent
                width: root.width
                height: root.height
                transform: Matrix4x4 { matrix: transformMatrix }
                radius: root.radius
                color: root.fillColor
            }

            // Reduces opacity of the dropped shadow
            Item {
                anchors.centerIn: parent
                width: parent.width + 16
                height: parent.height + 16
                opacity: 0.67
                z: -1

                // Acts as a shadow caster.
                // Putting content inside it is discouraged as antialiasing does not seem to be working in this case
                Item {
                    anchors.centerIn: parent
                    width: root.width
                    height: root.height

                    layer.enabled: root.enabled && root.shadowOpacity > 0.0
                    layer.effect: ElevationEffect { elevation: 16 }

                    transform: Matrix4x4 { matrix: transformMatrix }
                }
            }
        }
    }

    Component {
        id: complexComponent
        Item {
            id: complex
            // We have to make it wider due to using a layer
            // TODO: Remove this extra nesting, patch shape coords
            width: root.width + 2.0 * (Math.abs(halfLeftShift) + Math.abs(halfRightShift) + root.radius + 1.0)
            height: root.height

            readonly property real halfLeftShift: 0.5 * Math.tan(leftPartSkewDegrees * Math.PI / 180.0) * root.height
            readonly property real halfRightShift: 0.5 * Math.tan(rightPartSkewDegrees * Math.PI / 180.0) * root.height

            Item {
                anchors.fill: parent

                SlantedShape {
                    anchors.centerIn: parent
                    width: root.width
                    height: root.height
                    radius: root.radius
                    halfLeftShift: complex.halfLeftShift
                    halfRightShift: complex.halfRightShift
                    fillColor: root.fillColor
                    outlineColor: root.outlineColor
                }

                layer.enabled: true
                layer.samples: 4
            }

            Item {
                anchors.centerIn: parent
                width: parent.width + 16
                height: parent.height + 16
                opacity: 0.67 * root.shadowOpacity
                z: -1

                // Shadow
                SlantedShape {
                    id: shape
                    anchors.centerIn: parent
                    width: root.width - 2.0
                    height: root.height - 2.0
                    radius: root.radius
                    halfLeftShift: complex.halfLeftShift
                    halfRightShift: complex.halfRightShift
                    fillColor: Qt.rgba(0.0, 0.0, 0.0, 0.7)
                    outlineColor: Qt.rgba(0.0, 0.0, 0.0, 0.7)
                    //Doesn't work, hence we use DropShadow
                    //layer.enabled: root.enabled && root.shadowOpacity > 0.0
                    //layer.effect: ElevationEffect { elevation: 16 }
                }

                DropShadow {
                    anchors.fill: shape
                    source: shape
                    horizontalOffset: 8
                    verticalOffset: 8
                    radius: 16
                    samples: 16
                    // Try matching ElevationEffect density
                    color: Qt.rgba(0.0, 0.0, 0.0, 0.67 * (0.7 * root.shadowOpacity))
                }
            }
        }
    }
}