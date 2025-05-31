import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Column {
    id: root
    spacing: 12

    property var model
    property bool isInEditorMode
    property bool allowMultiBind
    property color highlightColor

    signal bindingRequested(int command)
    signal bindingSelected(int command)

    Repeater {
        model: root.model

        BindableCommand {
            text: modelData["text"]
            command: modelData["command"]
            commandNum: modelData["commandNum"]
            isBound: modelData["isBound"]
            isInEditorMode: root.isInEditorMode
            allowMultiBind: root.allowMultiBind
            highlightColor: root.highlightColor
            onBindingRequested: root.bindingRequested(command)
            onBindingSelected: root.bindingSelected(command)
        }
    }
}