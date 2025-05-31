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

    property var rowDataModel
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

    Connections {
        target: rowDataModel
        onSampleDataChanged: updateYValues()
    }

    onFixedVisualMinChanged: updateYValues()
    onFixedVisualMaxChanged: updateYValues()
    onUseFixedLevelIfSteadyChanged: updateYValues()
    onMinVisualFracChanged: updateYValues()
    onMaxVisualFracChanged: updateYValues()
    onSteadyVisualFracChanged: updateYValues()
    onDisplayLowerBarChanged: updateYValues()

    Component.onCompleted: {
        updateXValues()
        updateYValues()
    }
    onWidthChanged: {
        updateXValues()
        updateYValues()
    }
    onHeightChanged: {
        updateXValues()
        updateYValues()
    }

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
        text: valueFormatter(rowDataModel.displayedPeakMax)
        font.weight: Font.Bold
        font.family: Hud.ui.numbersFontFamily
        font.pointSize: 12
        width: 32
    }
    HudLabel {
        anchors.right: altTitleLabel.left
        anchors.top: parent.verticalCenter
        horizontalAlignment: Qt.AlignHCenter
        text: valueFormatter(rowDataModel.displayedPeakMin)
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
        Component.onCompleted: {
            updateXValues()
            updateYValues()
        }
        onWidthChanged: {
            updateXValues()
            updateYValues()
        }
        onHeightChanged: {
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
            PathLine {} PathLine {} PathLine {}
        }
        ShapePath {
            id: lowerBarPath
            fillColor: "transparent"
            strokeColor: "transparent"
            Behavior on strokeColor { ColorAnimation { duration: 250 } }
            strokeWidth: 2
            startX: 0
            startY: shape.height
            PathLine {
                x: shape.width
                y: shape.height
            }
        }
    }

    function updateXValues() {
        let count = path.pathElements.length
        for (let i = 0; i < count; ++i) {
            path.pathElements[i].x = (i + 1) * (shape.width / count);
        }
    }

    function updateYValues() {
        const count = rowDataModel.getSampleCount()
        // TODO: Don't check in release builds
        console.assert(steadyVisualFrac >= 0.0 && steadyVisualFrac <= 1.0)
        console.assert(minVisualFrac >= 0.0 && minVisualFrac < 1.0)
        console.assert(maxVisualFrac > 0.0 && maxVisualFrac <= 1.0)
        console.assert(minVisualFrac < maxVisualFrac)
        let minToUse = rowDataModel.actualMin
        if (typeof(fixedVisualMin) !== "undefined") {
            minToUse = fixedVisualMin
        }
        let maxToUse = rowDataModel.actualMax
        if (typeof(fixedVisualMax) !== "undefined") {
            maxToUse = fixedVisualMax
        }
        // Save resolved aliases to members/fields for performance reasons
        const _pathElements = path.pathElements
        const _shapeHeight  = shape.height
        const _model        = rowDataModel
        console.assert(count === _pathElements.length + 1)
        if (minToUse !== maxToUse && !(useFixedLevelIfSteady && _model.actualMin === _model.actualMax)) {
            console.assert(minToUse < maxToUse)
            const rcpDelta   = 1.0 / (maxToUse - minToUse)
            const _minFrac   = minVisualFrac
            const deltaFrac = maxVisualFrac - minVisualFrac
            let frac = (_model.sampleAt(0) - minToUse) * rcpDelta
            // Clip it
            frac = Math.min(1.0, Math.max(0.0, frac))
            // Level it
            frac = _minFrac + deltaFrac * frac
            // Calc flipped Y
            path.startY = _shapeHeight * (1.0 - frac)
            for (let i = 1; i < count; ++i) {
                frac = (_model.sampleAt(i) - minToUse) * rcpDelta
                // Clip it
                frac = Math.min(1.0, Math.max(0.0, frac))
                // Level it
                frac = _minFrac + deltaFrac * frac
                // Calc flipped Y
                _pathElements[i - 1].y = _shapeHeight * (1.0 - frac)
            }
            lowerBarPath.strokeColor = Hud.ui.colorWithAlpha(root.strokeColor, 0.2)
        } else {
            const resultY = (1.0 - steadyVisualFrac) * _shapeHeight
            path.startY = resultY
            for (let i = 1; i < count; ++i) {
                _pathElements[i - 1].y = resultY
            }
            lowerBarPath.strokeColor = "transparent"
        }
    }
}