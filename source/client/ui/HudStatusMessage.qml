import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root
    implicitWidth: label.implicitWidth + 48 * actualScale
    implicitHeight: label.implicitHeight + 48 * actualScale

    property var povDataModel
    property bool isMiniview
    property real miniviewScale: 1.0

    readonly property real defaultFontSize: 20
    readonly property real minFontSize: 11
    readonly property bool hasReachedDownscalingLimit: isMiniview ? (miniviewScale * defaultFontSize < minFontSize) : false
    readonly property real actualScale: hasReachedDownscalingLimit ? (minFontSize / defaultFontSize) : miniviewScale

    Connections {
        target: root.povDataModel
        onStatusMessageChanged: {
            if (popAnim.running) {
                popAnim.stop()
            }
            if (pushAnim.running) {
                pushAnim.stop()
            }
            label.text = statusMessage
            pushAnim.start()
            popTimer.start()
        }
    }
    Connections {
        target: Hud.ui
        enabled: !root.isMiniview
        onDisplayedHudItemsRetrievalRequested: Hud.ui.supplyDisplayedHudItemAndMargin(label, 4.0)
    }

    Timer {
        id: popTimer
        interval: 3000
        onTriggered: popAnim.start()
    }

    ParallelAnimation {
        id: pushAnim
        NumberAnimation {
            target: label
            property: "transformXScale"
            from: 0.0; to: 1.0
            easing.type: Easing.InOutElastic
            easing.amplitude: 5.0
            duration: 333
        }
        NumberAnimation {
            target: label
            property: "transformYScale"
            from: 0.0; to: 1.0
            easing.type: Easing.InOutElastic
            easing.amplitude: 5.0
            duration: 333
        }
    }

    ParallelAnimation {
        id: popAnim
        NumberAnimation {
            target: label
            property: "transformXScale"
            from: 1.0; to: 0.0
            easing.type: Easing.InCubic
            duration: 64
        }
        NumberAnimation {
            target: label
            property: "transformYScale"
            from: 1.0; to: 0.0
            easing.type: Easing.InCubic
            duration: 64
        }
    }

    Label {
        id: label
        width: implicitWidth
        height: implicitHeight
        anchors.centerIn: parent

        property real transformXScale
        property real transformYScale
        transform: Scale {
            origin.x: 0.5 * width
            origin.y: 0.5 * height
            xScale: label.transformXScale
            yScale: label.transformYScale
        }
        horizontalAlignment: Qt.AlignHCenter
        verticalAlignment: Qt.AlignVCenter
        font.weight: Font.Bold
        font.pointSize: defaultFontSize * actualScale
        font.letterSpacing: 2 * actualScale
        font.wordSpacing: 3 * actualScale
        font.capitalization: Font.SmallCaps
        textFormat: Text.StyledText
        style: hasReachedDownscalingLimit ? Text.Normal : Text.Raised
    }
}