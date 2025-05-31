import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Shapes 1.12
import net.warsow 2.6

Shape {
    id: root
    property bool isLeft
    property bool highlighted
    property color highlightColor
    property color defaultStrokeColor
    property real referenceSide

    readonly property real xSign: isLeft ? -1.0 : 1.0

    layer.enabled: true
    layer.samples: 4

    ShapePath {
        fillColor: "transparent"
        strokeColor: highlighted ?
            (highlightColor != Qt.rgba(0, 0, 0, 0) ? highlightColor : Material.accent) : defaultStrokeColor
        strokeWidth: 2
        startX: isLeft ? root.width - 3 : +3
        startY: 5
        PathCurve {
            relativeX: +xSign * 0.20 * referenceSide - xSign * 3
            relativeY: +0.05 * referenceSide
        }
        PathCurve {
            relativeX: +xSign * 0.10 * referenceSide - xSign * 3
            relativeY: +0.30 * referenceSide
        }
        PathCurve {
            relativeX: +xSign * 0.00 * referenceSide - 0
            relativeY: +0.20 * referenceSide
        }
        PathCurve {
            relativeX: -xSign * 0.20 * referenceSide + xSign * 3
            relativeY: -0.05 * referenceSide
        }
        PathArc {
            x: isLeft ? root.width - 3 : +3; y: 5
            radiusX: 300
            radiusY: 300
            direction: isLeft ? PathArc.Clockwise : PathArc.Counterclockwise
        }
    }
}