import QtQuick 2.12
import QtQuick.Controls 2.12
import net.warsow 2.6

StackView {
    readonly property int enterAnimDuration: 120
    readonly property int exitAnimDuration: 100

    pushEnter: Transition {
        NumberAnimation {
            property: "scale"
            from: 0.0; to: 1.0
            duration: enterAnimDuration
        }
        NumberAnimation {
            property: "opacity"
            from: 0.0; to: 1.0
            duration: enterAnimDuration
            easing.type: Easing.InCubic
        }
    }
    pushExit: Transition {
        NumberAnimation {
            property: "opacity"
            from: 0.0; to: 1.0
            duration: exitAnimDuration
        }
    }

    popEnter: pushEnter
    popExit: pushExit
    replaceEnter: pushEnter
    replaceExit: pushExit
}