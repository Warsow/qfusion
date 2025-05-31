import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

SequentialAnimation {
    id: root

    property int highlightInterval: 8000

    // Can't even declare an inner QtObject within SequentialAnimation (?)

    property bool _highlightActive: false
    property real _bodyShiftFrac: 0.0
    Behavior on _bodyShiftFrac { SmoothedAnimation { duration: 50 } }
    property int _colorAnimDuration: 50

    readonly property bool highlightActive: _highlightActive
    readonly property real bodyShiftFrac: _bodyShiftFrac
    readonly property int colorAnimDuration: _colorAnimDuration

    loops: Animation.Infinite
    PauseAnimation { duration: highlightInterval / 2 }
    PropertyAction { target: root; property: "_colorAnimDuration"; value: 200 }
    PropertyAction { target: root; property: "_highlightActive"; value: true }
    NumberAnimation { target: root; property: "_bodyShiftFrac"; from: 0.0; to: +1.0; duration: 200 }
    NumberAnimation { target: root; property: "_bodyShiftFrac"; from: +1.0; to: -0.6; duration: 200 }
    NumberAnimation { target: root; property: "_bodyShiftFrac"; from: -0.6; to: +0.6; duration: 200 }
    NumberAnimation { target: root; property: "_bodyShiftFrac"; from: +0.6; to: -0.3; duration: 200 }
    NumberAnimation { target: root; property: "_bodyShiftFrac"; from: -0.3; to: +0.3; duration: 200 }
    PropertyAction { target: root; property: "_highlightActive"; value: false }
    NumberAnimation { target: root; property: "_bodyShiftFrac"; from: +0.3; to: 0.0; duration: 300 }
    PropertyAction { target: root; property: "_colorAnimDuration"; value: 500 }
    PropertyAction { target: root; property: "_highlightActive"; value: false }
    PauseAnimation { duration: 500 }
    PropertyAction { target: root; property: "_colorAnimDuration"; value: 50 }
    PauseAnimation { duration: highlightInterval / 2 }

    onRunningChanged: {
        if (!running) {
            root._highlightActive   = false
            root._bodyShiftFrac     = 0.0
            root._colorAnimDuration = 50
        }
    }
}