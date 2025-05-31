import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Shapes 1.12


Shape {
    id: root
    property real radius
    property color outlineColor
    property color fillColor
    property real halfLeftShift
    property real halfRightShift

    ShapePath {
        // Top-left
        startX: +root.radius + halfLeftShift
        startY: +root.radius
        strokeWidth: 2 * root.radius
        strokeColor: root.outlineColor
        fillColor: root.fillColor
        joinStyle: ShapePath.RoundJoin
        // Top-right
        PathLine {
            x: -root.radius + root.width + halfRightShift
            y: +root.radius
        }
        // Bottom-right
        PathLine {
            x: -root.radius + root.width - halfRightShift
            y: -root.radius + root.height
        }
        // Bottom-left
        PathLine {
            x: +root.radius - halfLeftShift
            y: -root.radius + root.height
        }
        // Top-left
        PathLine {
            x: +root.radius + halfLeftShift
            y: +root.radius
        }
    }
}