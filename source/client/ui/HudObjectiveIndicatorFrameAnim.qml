import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

ParallelAnimation {
    id: root
    property real initialWidth
    property real initialHeight
    property real peakWidth
    property real peakHeight
    property real finalWidth
    property real finalHeight
    property real minOpacity
    property int step1Duration
    property int step2Duration
    property var target

    loops: Animation.Infinite

    SequentialAnimation {
        ParallelAnimation {
            PropertyAnimation {
                target: root.target
                property: "width"
                from: initialWidth
                to: peakWidth
                duration: step1Duration
            }
            PropertyAnimation {
                target: root.target
                properties: "height"
                from: initialHeight
                to: peakHeight
                duration: step1Duration
            }
        }
        ParallelAnimation {
            PropertyAnimation {
                target: root.target
                property: "width"
                from: peakWidth
                to: finalWidth
                duration: step2Duration
            }
            PropertyAnimation {
                target: root.target
                property: "height"
                from: peakHeight
                to: finalHeight
                duration: step2Duration
            }
        }
    }
    SequentialAnimation {
        PropertyAnimation {
            target: root.target
            property: "baseOpacity"
            from: 1.0
            to: minOpacity
            duration: step1Duration
        }
        PropertyAnimation {
            target: root.target
            property: "baseOpacity"
            from: minOpacity
            to: 1.0
            duration: step2Duration
        }
    }
}