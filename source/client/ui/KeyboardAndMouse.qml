import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root

    implicitHeight: mainKeyboardPane.height + arrowPadPane.height + arrowPadPane.anchors.topMargin

    signal bindingRequested(int quakeKey)
    signal unbindingRequested(int quakeKey)
    signal keySelected(int quakeKey)

    property bool isInEditorMode

    KeyboardPane {
        id: mainKeyboardPane
        isInEditorMode: root.isInEditorMode

        anchors {
            top: parent.top
            horizontalCenter: parent.horizontalCenter
        }

        width: parent.width
        rowHeight: 26
        rowModels: [
            UI.keysAndBindings.keyboardMainPadRow1,
            UI.keysAndBindings.keyboardMainPadRow2,
            UI.keysAndBindings.keyboardMainPadRow3,
            UI.keysAndBindings.keyboardMainPadRow4,
            UI.keysAndBindings.keyboardMainPadRow5,
            UI.keysAndBindings.keyboardMainPadRow6
        ]

        onBindingRequested: root.bindingRequested(quakeKey)
        onUnbindingRequested: root.unbindingRequested(quakeKey)
        onKeySelected: root.keySelected(quakeKey)
    }

    MouseKeysPane {
        height: arrowPadPane.height
        isInEditorMode: root.isInEditorMode

        anchors {
            top: mainKeyboardPane.bottom
            topMargin: 24
            left: mainKeyboardPane.left
            right: arrowPadPane.left
            rightMargin: 32
        }

        onBindingRequested: root.bindingRequested(quakeKey)
        onUnbindingRequested: root.unbindingRequested(quakeKey)
        onKeySelected: root.keySelected(quakeKey)
    }

    KeyboardPane {
        id: arrowPadPane
        isInEditorMode: root.isInEditorMode

        anchors {
            top: mainKeyboardPane.bottom
            topMargin: 24
            right: numPadPane.left
            rightMargin: 32
        }

        width: mainKeyboardPane.width / 5
        rowHeight: 26
        rowModels: [
            UI.keysAndBindings.keyboardArrowPadRow1,
            UI.keysAndBindings.keyboardArrowPadRow2,
            UI.keysAndBindings.keyboardArrowPadRow3,
            UI.keysAndBindings.keyboardArrowPadRow4,
            UI.keysAndBindings.keyboardArrowPadRow5
        ]

        onBindingRequested: root.bindingRequested(quakeKey)
        onUnbindingRequested: root.unbindingRequested(quakeKey)
        onKeySelected: root.keySelected(quakeKey)
    }

    KeyboardPane {
        id: numPadPane
        isInEditorMode: root.isInEditorMode

        anchors {
            top: mainKeyboardPane.bottom
            topMargin: 24
            right: mainKeyboardPane.right
        }

        // One extra key
        width: 4 * arrowPadPane.width / 3
        rowHeight: 26
        rowModels: [
            UI.keysAndBindings.keyboardNumPadRow1,
            UI.keysAndBindings.keyboardNumPadRow2,
            UI.keysAndBindings.keyboardNumPadRow3,
            UI.keysAndBindings.keyboardNumPadRow4,
            UI.keysAndBindings.keyboardNumPadRow5,
        ]

        onBindingRequested: root.bindingRequested(quakeKey)
        onUnbindingRequested: root.unbindingRequested(quakeKey)
        onKeySelected: root.keySelected(quakeKey)
    }
}