import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Shapes 1.12
import net.warsow 2.6

Item {
    id: root

    property alias title: titleLabel.text
    property alias altTitle: altTitleLabel.text
    property alias primaryValueText: primaryValueLabel.text
    property alias strokeColor: path.strokeColor

    property var rowData
    property var fixedVisualMin: undefined
    property var fixedVisualMax: undefined
    property real minVisualFrac: 0.0
    property real maxVisualFrac: 1.0
    property bool useFixedLevelIfSteady: false
    // This property is meaningful even if useFixedLevelIfSteady is not set
    // (we have to show it on this level if no fixed bounds are defined)
    property real steadyVisualFrac: 0.5
    property bool displayLowerBar: false
    // Let us just skip the upper bar for now

    property var valueFormatter: (v) => '' + v

    onRowDataChanged: updateYValues()
    onFixedVisualMinChanged: updateYValues()
    onFixedVisualMaxChanged: updateYValues()
    onUseFixedLevelIfSteadyChanged: updateYValues()
    onDisplayLowerBarChanged: updateYValues()

    HudLabel {
        id: titleLabel
        anchors.left: parent.left
        anchors.bottom: parent.verticalCenter
        horizontalAlignment: Qt.AlignHCenter
        font.weight: Font.Bold
        font.family: Hud.ui.numbersFontFamily
        font.pointSize: 12
        textFormat: Text.StyledText
        width: 40
        opacity: 0.7
    }
    HudLabel {
        id: primaryValueLabel
        anchors.left: parent.left
        anchors.top: parent.verticalCenter
        horizontalAlignment: Qt.AlignHCenter
        font.weight: Font.Bold
        font.family: Hud.ui.numbersFontFamily
        font.pointSize: 12
        textFormat: Text.PlainText
        width: 40
    }

    HudLabel {
        id: maxLabel
        anchors.right: altTitleLabel.left
        anchors.bottom: parent.verticalCenter
        horizontalAlignment: Qt.AlignHCenter
        text: valueFormatter(rowData.displayedPeakMax)
        font.weight: Font.Bold
        font.family: Hud.ui.numbersFontFamily
        font.pointSize: 12
        width: 32
    }
    HudLabel {
        anchors.right: altTitleLabel.left
        anchors.top: parent.verticalCenter
        horizontalAlignment: Qt.AlignHCenter
        text: valueFormatter(rowData.displayedPeakMin)
        font.weight: Font.Bold
        font.family: Hud.ui.numbersFontFamily
        font.pointSize: 12
        width: 32
    }
    HudLabel {
        id: altTitleLabel
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        font.weight: Font.Bold
        font.family: Hud.ui.numbersFontFamily
        font.pointSize: 12
        textFormat: Text.StyledText
        width: 40
        opacity: 0.7
    }

    Shape {
        id: shape
        vendorExtensionsEnabled: false
        anchors.left: titleLabel.right
        anchors.right: maxLabel.left
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.margins: 8
        onWidthChanged: {
            updateXValues()
            updateYValues()
        }
        Component.onCompleted: {
            updateXValues()
            updateYValues()
        }
        ShapePath {
            id: path
            fillColor: "transparent"
            strokeWidth: 2
            // Unable to use PathPolyline in 2.12
            PathLine {} PathLine {} PathLine {} PathLine {}
            PathLine {} PathLine {} PathLine {} PathLine {}
            PathLine {} PathLine {} PathLine {} PathLine {}
            PathLine {} PathLine {} PathLine {}
        }
        ShapePath {
            id: lowerBarPath
            fillColor: "transparent"
            strokeColor: "transparent"
            Behavior on strokeColor { ColorAnimation { duration: 250 } }
            strokeWidth: 2
            PathLine {}
        }
    }

    function updateXValues() {
        let count = path.pathElements.length
        for (let i = 0; i < count; ++i) {
            path.pathElements[i].x = (i + 1) * (shape.width / count);
        }
    }

    function updateYValues() {
        const count = rowData.samples.length
        console.assert(count === path.pathElements.length + 1)
        console.assert(steadyVisualFrac >= 0.0 && steadyVisualFrac <= 1.0)
        console.assert(minVisualFrac >= 0.0 && minVisualFrac < 1.0)
        console.assert(maxVisualFrac > 0.0 && maxVisualFrac <= 1.0)
        console.assert(minVisualFrac < maxVisualFrac)
        let minToUse = rowData.actualMin
        if (typeof(fixedVisualMin) !== "undefined") {
            minToUse = fixedVisualMin
        }
        let maxToUse = rowData.actualMax
        if (typeof(fixedVisualMax) !== "undefined") {
            maxToUse = fixedVisualMax
        }
        if (minToUse !== maxToUse && !(useFixedLevelIfSteady && rowData.actualMin === rowData.actualMax)) {
            console.assert(minToUse < maxToUse)
            const rcpDelta = 1.0 / (maxToUse - minToUse)
            let frac = (rowData.samples[0] - minToUse) * rcpDelta
            // Clip it
            frac = Math.min(1.0, Math.max(0.0, frac))
            // Level it
            frac = minVisualFrac + (maxVisualFrac - minVisualFrac) * frac
            // Calc flipped Y
            path.startY = shape.height * (1.0 - frac)
            for (let i = 1; i < count; ++i) {
                frac = (rowData.samples[i] - minToUse) * rcpDelta
                // Clip it
                frac = Math.min(1.0, Math.max(0.0, frac))
                // Level it
                frac = minVisualFrac + (maxVisualFrac - minVisualFrac) * frac
                // Calc flipped Y
                path.pathElements[i - 1].y = shape.height * (1.0 - frac)
            }
            lowerBarPath.strokeColor       = UI.ui.colorWithAlpha(root.strokeColor, 0.2)
            lowerBarPath.startX            = 0
            lowerBarPath.startY            = shape.height * (1.0 - minVisualFrac)
            lowerBarPath.pathElements[0].x = shape.width
            lowerBarPath.pathElements[0].y = shape.height * (1.0 - minVisualFrac)
        } else {
            const frac = 1.0 - steadyVisualFrac
            path.startY = frac * shape.height
            for (let i = 1; i < count; ++i) {
                path.pathElements[i - 1].y = frac * shape.height
            }
            lowerBarPath.strokeColor = "transparent"
        }
    }
}