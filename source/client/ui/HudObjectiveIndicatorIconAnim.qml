import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

SequentialAnimation {
    id: root

    property var target
    property string targetProperty
    property real originalValue
    property real valueShiftAmplitude
    property int period: 500

    loops: Animation.Infinite

    NumberAnimation {
        target: root.target
        property: targetProperty
        duration: 0.25 * period
        from: originalValue
        to: originalValue - valueShiftAmplitude
    }
    NumberAnimation {
        target: root.target
        property: targetProperty
        duration: 0.5 * period
        from: originalValue - valueShiftAmplitude
        to: originalValue + valueShiftAmplitude
    }
    NumberAnimation {
        target: root.target
        property: targetProperty
        duration: 0.25 * period
        from: originalValue + valueShiftAmplitude
        to: originalValue
    }
}