import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

RowLayout {
    id: root
    spacing: 4
    height: 28
    clip: false

    property real rowSpacing
    property var model
    property bool isInEditorMode

    signal bindingRequested(int quakeKey)
    signal unbindingRequested(int quakeKey)
    signal keySelected(int quakeKey)

    Repeater {
        model: root.model

        delegate: KeyboardKey {
            rowSpacing: root.rowSpacing
            text: modelData["text"]
            quakeKey: modelData["quakeKey"]
            enabled: modelData["enabled"]
            hidden: modelData["hidden"]
            rowSpan: modelData["rowSpan"]
            group: modelData["group"]
            isInEditorMode: root.isInEditorMode
            Layout.fillWidth: true
            Layout.preferredWidth: modelData["layoutWeight"]
            onBindingRequested: root.bindingRequested(quakeKey)
            onUnbindingRequested: root.unbindingRequested(quakeKey)
            onKeySelected: root.keySelected(quakeKey)
        }
    }
}