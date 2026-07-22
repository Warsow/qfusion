import QtQuick 2.12
import QtQuick.Controls 2.12

// Looks like it has to be an Item, not just QtObject
Item {
    // Negative if hidden by default until explicit show() call
    property int appearDelay: 75
    property var targets

    readonly property var _animTarget : targets ? null : parent
    readonly property var _animTargets: targets ? targets : []

    readonly property bool animating: timer.running || appearAnim.running || fadeOutAnim.running

    Component.onCompleted: {
        if (targets) {
            for (const target of targets) {
                target.scale = target.opacity = 0.0
            }
        } else {
            parent.scale = parent.opacity = 0.0
        }
        if (appearDelay > 0) {
            timer.interval = appearDelay
            timer.start()
        } else if (appearDelay === 0) {
            appearAnim.start()
        }
    }

    Timer {
        id: timer
        onTriggered: appearAnim.start()
    }

    ParallelAnimation {
        id: appearAnim
        NumberAnimation {
            target: _animTarget
            targets: _animTargets
            property: "scale"
            from: 0.0
            to: 1.0
            duration: 100
            easing.type: Easing.InCubic
        }
        NumberAnimation {
            target: _animTarget
            targets: _animTargets
            property: "opacity"
            from: 0.0
            to: 1.0
            duration: 100
            easing.type: Easing.InCubic
        }
    }

    SequentialAnimation {
        id: fadeOutAnim
        ScriptAction {
            script: opacity = Math.min(opacity, 0.7)
        }
        NumberAnimation {
            target: _animTarget
            targets: _animTargets
            property: "opacity"
            from: 0.7
            to: 0.0
            duration: 100
        }
    }

    NumberAnimation {
        id: shrinkAnim
        target: _animTarget
        targets: _animTargets
        property: "scale"
        from: 1.0
        to: 0.0
        duration: 100
    }

    NumberAnimation {
        id: expandAnim
        target: _animTarget
        targets: _animTargets
        property: "scale"
        from: 1.0
        to: 2.0
        duration: 100
    }

    function show() {
        timer.stop()
        fadeOutAnim.stop()
        shrinkAnim.stop()
        expandAnim.stop()
        appearAnim.start()
    }

    function expandAndHide() {
        timer.stop()
        appearAnim.stop()
        shrinkAnim.stop()
        fadeOutAnim.start()
        expandAnim.start()
    }

    function shrinkAndHide() {
        timer.stop()
        appearAnim.stop()
        expandAnim.stop()
        fadeOutAnim.start()
        shrinkAnim.start()
    }

    // Helpers for making stack view status handling in actual pages one-liners

    function expandAndHideIfDeactivating(status) {
        if (status === StackView.Deactivating) {
            expandAndHide()
        }
    }

    function shrinkAndHideIfDeactivating(status) {
        if (status === StackView.Deactivating) {
            shrinkAndHide()
        }
    }
}