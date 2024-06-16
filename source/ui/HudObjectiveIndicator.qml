import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    visible: indicatorState.enabled
    implicitWidth: visible ? (collapsedHeight + extraExclusiveModeWidth) : 0
    implicitHeight: visible ? (collapsedHeight + barHeight + 1.5 * barMargin) : 0
    
    property var commonDataModel

    readonly property real extraExclusiveModeWidth: useExclusiveMode ?
        Math.max((indicatorState.iconNum ? 144 : 144 - 32), 16 + textMetrics.width) : 0

    readonly property real alertAnimStep1Duration: 125
    readonly property real alertAnimStep2Duration: 333

    readonly property real actionAnimStep1Duration: 175
    readonly property real actionAnimStep2Duration: 400

    readonly property real smoothAnimDuration: 50

    // Smooth the width transition
    Behavior on implicitWidth { SmoothedAnimation { duration: smoothAnimDuration } }

    property var indicatorState
    property real barHeightFrac
    property bool useExclusiveMode
    // The indicatorState.stringNum property does not singal updates!
    // TODO: Expose indicator states as QAbstractItemModel?
    property bool canUseExclusiveMode: indicatorState && root.commonDataModel.getIndicatorStatusString(indicatorState.stringNum) != ""
    onIndicatorStateChanged: {
        canUseExclusiveMode = indicatorState && root.commonDataModel.getIndicatorStatusString(indicatorState.stringNum) != ""
    }

    TextMetrics {
        id: textMetrics
        font: statusStringLabel.font
        elide: statusStringLabel.elide
        text: statusStringLabel.text
    }

    readonly property color indicatorColor: indicatorState.color
    readonly property int indicatorProgress: indicatorState.progress
    readonly property int indicatorAnim: indicatorState.anim

    readonly property real minFrameBaseOpacity: 0.5
    readonly property real maxFrameWidth: Hud.teamScoreHeight - 2 * Hud.elementRadius + extraExclusiveModeWidth
    readonly property real maxFrameHeight: Hud.teamScoreHeight - 2 * Hud.elementRadius
    readonly property real collapsedHeight: Hud.teamScoreHeight
    readonly property real borderWidth: Hud.miniviewBorderWidth

    readonly property real barHeight: 16 * barHeightFrac
    readonly property real barMargin: 8 * barHeightFrac

    Item {
        id: frameArea
        anchors.verticalCenter: parent.top
        anchors.verticalCenterOffset: 0.5 * collapsedHeight
        anchors.horizontalCenter: parent.horizontalCenter
        width: collapsedHeight
        height: collapsedHeight
    }

    Rectangle {
        id: frame
        property real baseOpacity: minFrameBaseOpacity
        anchors.centerIn: frameArea
        width: maxFrameWidth
        height: maxFrameHeight
        // Match smoothing the width transition of the root element
        Behavior on width { SmoothedAnimation { duration: smoothAnimDuration } }
        Behavior on height { SmoothedAnimation { duration: smoothAnimDuration } }
        color: "transparent"
        border.color: indicatorColor
        border.width: borderWidth
        opacity: Math.min(1.0, baseOpacity + (Hud.ui.isShowingScoreboard ? 0.0 : 0.1))
        radius: 1.2 * Hud.elementRadius

        function restoreDefaultProperties() {
            if (!frameAlertAnim.running && !frameActionAnim.running) {
                frame.width       = maxFrameWidth
                frame.height      = maxFrameHeight
                frame.baseOpacity = minFrameBaseOpacity
            }
        }
    }

    RowLayout {
        id: innerContent
        anchors.horizontalCenter: frameArea.horizontalCenter
        anchors.verticalCenter: frameArea.verticalCenter
        anchors.verticalCenterOffset: (useExclusiveMode ? -2 : 0)
        spacing: 5

        function restoreDefaultProperties() {
            if (!innerContentAlertAnim.running) {
                innerContent.anchors.horizontalCenterOffset = 0
            }
            if (!innerContentActionAnim.running) {
                innerContent.anchors.verticalCenterOffset = (useExclusiveMode ? -2 : 0)
            }
            if (!innerContentAnimExclusive.running) {
                innerContent.scale = 1.0
            }
        }

        Image {
            visible: indicatorState.iconNum
            Layout.preferredWidth: 32
            Layout.preferredHeight: 32
            Layout.alignment: Qt.AlignVCenter
            smooth: true
            mipmap: true
            source: root.commonDataModel.getIndicatorIconPath(indicatorState.iconNum)
        }

        Label {
            id: statusStringLabel
            text: root.commonDataModel.getIndicatorStatusString(indicatorState.stringNum)
            visible: useExclusiveMode
            Layout.alignment: Qt.AlignVCenter
            font.weight: Font.Bold
            font.capitalization: Font.AllUppercase
            font.family: Hud.ui.headingFontFamily
            font.pointSize: 15
            font.letterSpacing: 1.0
            style: Text.Raised
        }
    }

    // Changing anim duration on the fly does not seem to work, declare mutually exclusive instances as a workaround

    HudObjectiveIndicatorIconAnim {
        id: innerContentAlertAnim
        target: innerContent
        valueShiftAmplitude: 2.00
        originalValue: 0.0
        period: alertAnimStep1Duration + alertAnimStep2Duration
        running: indicatorAnim === HudCommonDataModel.AlertAnim && !useExclusiveMode
        targetProperty: "anchors.verticalCenterOffset"
        onRunningChanged: innerContent.restoreDefaultProperties()
    }

    HudObjectiveIndicatorIconAnim {
        id: innerContentActionAnim
        target: innerContent
        valueShiftAmplitude: 1.25
        originalValue: 0.0
        period: actionAnimStep1Duration + actionAnimStep2Duration
        running: indicatorAnim === HudCommonDataModel.ActionAnim && !useExclusiveMode
        targetProperty: "anchors.verticalCenterOffset"
        onRunningChanged: innerContent.restoreDefaultProperties()
    }

    SequentialAnimation {
        id: innerContentAnimExclusive
        loops: Animation.Infinite
        running: (indicatorAnim === HudCommonDataModel.AlertAnim || indicatorAnim === HudCommonDataModel.ActionAnim) && useExclusiveMode
        NumberAnimation {
            target: innerContent
            property: "scale"
            from: 0.00
            to: 1.00
            duration: indicatorAnim === HudCommonDataModel.AlertAnim ? alertAnimStep1Duration : actionAnimStep1Duration
        }
        NumberAnimation {
            target: innerContent
            property: "scale"
            from: 1.00
            to: 0.90
            duration: indicatorAnim === HudCommonDataModel.AlertAnim ? alertAnimStep2Duration : actionAnimStep2Duration
        }
        onRunningChanged: innerContent.restoreDefaultProperties()
    }

    HudObjectiveIndicatorFrameAnim {
        id: frameAlertAnim
        target: frame
        running: indicatorAnim === HudCommonDataModel.AlertAnim
        initialWidth: 0.5 * maxFrameWidth
        initialHeight: 0.5 * maxFrameHeight
        peakWidth: maxFrameWidth
        peakHeight: maxFrameHeight
        finalWidth: maxFrameWidth - 4
        finalHeight: maxFrameHeight - 4
        minOpacity: minFrameBaseOpacity
        step1Duration: alertAnimStep1Duration
        step2Duration: alertAnimStep2Duration
        onRunningChanged: frame.restoreDefaultProperties()
    }

    HudObjectiveIndicatorFrameAnim {
        id: frameActionAnim
        target: frame
        running: indicatorAnim === HudCommonDataModel.ActionAnim
        initialWidth: maxFrameWidth - 8
        initialHeight: maxFrameHeight - 8
        peakWidth: maxFrameWidth
        peakHeight: maxFrameHeight
        finalWidth: maxFrameWidth - 4
        finalHeight: maxFrameHeight - 4
        minOpacity: minFrameBaseOpacity
        step1Duration: actionAnimStep1Duration
        step2Duration: actionAnimStep2Duration
        onRunningChanged: frame.restoreDefaultProperties()
    }

    Rectangle {
        id: bar
        anchors.bottom: parent.bottom
        anchors.bottomMargin: root.barMargin
        anchors.horizontalCenter: parent.horizontalCenter
        width: parent.width - 2 * root.barMargin
        height: root.barHeight
        color: Hud.ui.colorWithAlpha(indicatorColor, 0.6)
        radius: 0.5 * Hud.elementRadius

        Rectangle {
            visible: indicatorProgress
            anchors.verticalCenter: parent.verticalCenter
            anchors.left: parent.left
            width: parent.width * 0.01 * Math.abs(indicatorProgress)
            height: parent.height
            color: indicatorColor
            Behavior on width { SmoothedAnimation { duration: 67 } }
        }
    }
}