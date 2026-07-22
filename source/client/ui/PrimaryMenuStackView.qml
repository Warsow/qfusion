import QtQuick 2.12
import QtQuick.Controls 2.12
import net.warsow 2.6

StackView {
    readonly property int enterAnimDuration: 100
    readonly property int enterAnimDelay: 25
    readonly property int exitAnimDuration: 80
    readonly property int enterAnimEasing: Easing.InQuad
    readonly property int exitAnimEasing: Easing.OutQuad

    pushEnter: Transition {
        SequentialAnimation {
            // ScriptAction + PauseAnimation do not work...
            NumberAnimation {
                property: "scale"
                from: 0.0; to: 0.0
                duration: enterAnimDelay
            }
            NumberAnimation {
                property: "scale"
                from: 0.0; to: 1.0
                duration: enterAnimDuration
                easing.type: enterAnimEasing
            }
        }
        SequentialAnimation {
            NumberAnimation {
                property: "opacity"
                from: 0.0; to: 0.0
                duration: enterAnimDelay
            }
            NumberAnimation {
                property: "opacity"
                from: 0.0; to: 1.0
                duration: enterAnimDuration
                easing.type: enterAnimEasing
            }
        }
    }
    pushExit: Transition {
        NumberAnimation {
            property: "scale"
            from: 1.0; to: 0.9
            duration: exitAnimDuration
            easing.type: exitAnimEasing
        }
        NumberAnimation {
            property: "opacity"
            from: 1.0; to: 0.0
            duration: exitAnimDuration
            easing.type: exitAnimEasing
        }
    }

    popEnter: pushEnter
    popExit: pushExit
    replaceEnter: pushEnter
    replaceExit: pushExit
}