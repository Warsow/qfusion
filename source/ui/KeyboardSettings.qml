import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    Component.onCompleted: UI.keysAndBindings.startTrackingUpdates()
    Component.onDestruction: UI.keysAndBindings.stopTrackingUpdates()

    property int pendingKeyToBind: 0
    // TODO: Make feasible commands start from 1
    property int pendingCommandToBind: -1

    Translate {
        id: keysGroupTransform
        y: 0
    }

    readonly property int headingFontWeight: Font.Medium
    readonly property int headingLetterSpacing: 1
    readonly property int headingFontSize: 12

    // TODO: Generalize?
    Loader {
        id: keysHeaderLoader
        active: pendingCommandToBind >= 0 && !isAnimating
        width: root.width
        anchors.bottom: keys.top
        anchors.bottomMargin: 64
        anchors.horizontalCenter: parent.horizontalCenter
        transform: keysGroupTransform

        // Just label for now. This is going to be extended by the "direct mode" switch
        sourceComponent: ColumnLayout {
            opacity: 0
            NumberAnimation on opacity {
                from: 0.0; to: 1.0
                duration: 100
                easing.type: Easing.InQuad
            }

            Label {
                Layout.alignment: Qt.AlignHCenter
                text: 'Select a key to bind the command <b><font color="orange">' +
                        UI.keysAndBindings.getCommandNameToDisplay(pendingCommandToBind) +
                        '</font></b> to. Press <b><font color="orange">ESC</font></b> to cancel.'
                font.weight: headingFontWeight
                font.pointSize: headingFontSize
                font.letterSpacing: headingLetterSpacing
            }
        }
    }

    KeyboardAndMouse {
        id: keys
        width: parent.width - 96
        height: implicitHeight
        anchors.horizontalCenter: parent.horizontalCenter
        transform: keysGroupTransform
        isInEditorMode: pendingCommandToBind >= 0
        onUnbindingRequested: {
            if (!isAnimating) {
                UI.keysAndBindings.unbind(quakeKey)
            }
        }
        onBindingRequested: {
            if (!isAnimating) {
                pendingKeyToBind = quakeKey
            }
        }
        onKeySelected: {
            if (!isAnimating) {
                const command = pendingCommandToBind
                pendingCommandToBind = -1
                UI.keysAndBindings.bind(quakeKey, command)
            }
        }
    }

    Translate {
        id: commandsGroupTransform
        y: 0
    }

    Loader {
        id: commandsHeaderLoader
        active: pendingKeyToBind && !isAnimating
        width: root.width
        anchors.bottom: commandsPane.top
        anchors.bottomMargin: 64
        anchors.horizontalCenter: parent.horizontalCenter
        transform: commandsGroupTransform

        sourceComponent: ColumnLayout {
            readonly property bool allowMultiBind: multiBindCheckBox.checked
            spacing: 16
            opacity: 0
                NumberAnimation on opacity {
                from: 0.0; to: 1.0
                duration: 100
                easing.type: Easing.InQuad
            }

            Label {
                Layout.alignment: Qt.AlignHCenter
                text: 'Select a command to bind to the key <b><font color="orange">' +
                        UI.keysAndBindings.getKeyNameToDisplay(pendingKeyToBind) +
                        '</font></b>. Press <b><font color="orange">ESC</font></b> to cancel.'
                font.weight: headingFontWeight
                font.pointSize: headingFontSize
                font.letterSpacing: headingLetterSpacing
            }
            WswCheckBox {
                id: multiBindCheckBox
                Layout.alignment: Qt.AlignLeft
                Layout.leftMargin: 96 // wtf?
                Material.theme: Material.Dark
                Material.foreground: "white"
                Material.accent: "orange"
                text: "Allow binding multiple keys to commands"
            }
        }
    }

    CommandsPane {
        id: commandsPane
        anchors.top: keys.bottom
        anchors.topMargin: 64
        anchors.horizontalCenter: parent.horizontalCenter
        transform: commandsGroupTransform
        isInEditorMode: pendingKeyToBind
        allowMultiBind: commandsHeaderLoader.item && commandsHeaderLoader.item.allowMultiBind
        onBindingRequested: {
            if (!isAnimating) {
                pendingCommandToBind = command
            }
        }
        onBindingSelected: {
            if (!isAnimating) {
                const key = pendingKeyToBind
                pendingKeyToBind = 0
                UI.keysAndBindings.bind(key, command)
            }
        }
    }

    onPendingKeyToBindChanged: {
        if (pendingKeyToBind) {
            returnKeysAnim.stop()
            returnCommandsAnim.stop()
            hideKeysAnim.start()
            liftCommandsAnim.start()
        } else {
            hideKeysAnim.stop()
            liftCommandsAnim.stop()
            returnKeysAnim.start()
            returnCommandsAnim.start()
        }
    }

    onPendingCommandToBindChanged: {
        if (pendingCommandToBind >= 0) {
            returnKeysAnim.stop()
            returnCommandsAnim.stop()
            dropKeysAnim.start()
            hideCommandsAnim.start()
        } else {
            dropKeysAnim.stop()
            hideCommandsAnim.stop()
            returnKeysAnim.start()
            returnCommandsAnim.start()
        }
    }

    readonly property bool isAnimatingKeys:
        hideKeysAnim.running || dropKeysAnim.running || returnKeysAnim.running
    readonly property bool isAnimatingCommands:
        hideCommandsAnim.running || liftCommandsAnim.running || returnCommandsAnim.running
    readonly property bool isAnimating: isAnimatingKeys || isAnimatingCommands

    NumberAnimation {
        id: hideKeysAnim
        target: keysGroupTransform
        property: "y"
        from: 0; to: -9999
        duration: 333
        easing.type: Easing.OutQuad
    }

    NumberAnimation {
        id: returnKeysAnim
        target: keysGroupTransform
        property: "y"
        from: -9999; to: 0
        duration: 333
        easing.type: Easing.OutQuad
    }

    NumberAnimation {
        id: dropKeysAnim
        target: keysGroupTransform
        property: "y"
        from: 0; to: 200
        duration: 200
        easing.type: Easing.OutQuad
    }

    NumberAnimation {
        id: liftCommandsAnim
        target: commandsGroupTransform
        property: "y"
        from: 0; to: -200
        duration: 200
        easing.type: Easing.OutQuad
    }

    NumberAnimation {
        id: returnCommandsAnim
        target: commandsGroupTransform
        property: "y"
        from: -200; to: 0
        duration: 200
        easing.type: Easing.OutQuad
    }

    NumberAnimation {
        id: hideCommandsAnim
        target: commandsGroupTransform
        property: "y"
        from: 0; to: 9999
        duration: 333
        easing.type: Easing.OutQuad
    }

    function handleKeyEvent(event) {
        if (event.key === Qt.Key_Escape) {
            // Let the animation complete itself
            if (isAnimating) {
                UI.ui.playBackSound()
                event.accepted = true
                return true
            }
            if (pendingKeyToBind || pendingCommandToBind >= 0) {
                UI.ui.playBackSound()
                pendingKeyToBind = 0
                pendingCommandToBind = -1
                event.accepted = true
                return true
            }

        }
        return false
    }
}
