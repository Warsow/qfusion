import QtQuick 2.12
import QtQuick.Controls 2.12

StackView {
    id: swipeView

    readonly property int enterAnimDuration: 190
    readonly property int exitAnimDuration: 190
    readonly property int enterAnimEasing: Easing.InQuad
    readonly property int exitAnimEasing: Easing.OutQuad

    readonly property real minScale: 0.8

    function switchRightTo(component, props) {
        replace(null, component, props || {}, StackView.PushTransition)
    }
    function switchLeftTo(component, props) {
        replace(null, component, props || {}, StackView.PopTransition)
    }

    pushEnter: Transition {
        NumberAnimation {
            property: "x"
            from: +swipeView.width; to: 0
            duration: enterAnimDuration
            easing.type: enterAnimEasing
        }
        NumberAnimation {
            property: "opacity"
            from: 0.0; to: 1.0
            duration: enterAnimDuration
            easing.type: enterAnimEasing
        }
        NumberAnimation {
            property: "scale"
            from: minScale; to: 1.0
            duration: enterAnimDuration
            easing.type: enterAnimEasing
        }
    }
    pushExit: Transition {
        NumberAnimation {
            property: "x"
            from: 0; to: -swipeView.width
            duration: exitAnimDuration
            easing.type: exitAnimEasing
        }
        NumberAnimation {
            property: "opacity"
            from: 1.0; to: 0.0
            duration: exitAnimDuration
            easing.type: exitAnimEasing
        }
        NumberAnimation {
            property: "scale"
            from: 1.0; to: minScale
            duration: exitAnimDuration
            easing.type: exitAnimEasing
        }
    }

    popEnter: Transition {
        NumberAnimation {
            property: "x"
            from: -swipeView.width; to: 0
            duration: enterAnimDuration
            easing.type: enterAnimEasing
        }
        NumberAnimation {
            property: "opacity"
            from: 0.0; to: 1.0
            duration: enterAnimDuration
            easing.type: enterAnimEasing
        }
        NumberAnimation {
            property: "scale"
            from: minScale; to: 1.0
            duration: enterAnimDuration
            easing.type: enterAnimEasing
        }
    }
    popExit: Transition {
        NumberAnimation {
            property: "x"
            from: 0; to: +swipeView.width
            duration: exitAnimDuration
            easing.type: exitAnimEasing
        }
        NumberAnimation {
            property: "opacity"
            from: 1.0; to: 0.0
            duration: exitAnimDuration
            easing.type: exitAnimEasing
        }
        NumberAnimation {
            property: "scale"
            from: 1.0; to: minScale
            duration: exitAnimDuration
            easing.type: exitAnimEasing
        }
    }

    replaceEnter: pushEnter
    replaceExit: pushExit
}