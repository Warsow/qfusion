import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Rectangle {
    id: root
    width: parent.width + 12
    height: layout.implicitHeight + 12
    color: Qt.rgba(0, 0, 0, 0.2)
    radius: 6

    property real rowHeight: 32
    property var rowModels
    property bool isInEditorMode

    signal bindingRequested(int quakeKey)
    signal unbindingRequested(int quakeKey)
    signal keySelected(int quakeKey)

    ColumnLayout {
        id: layout
        anchors.centerIn: parent
        width: parent.width - 12

        Repeater {
            model: rowModels.length
            KeyboardRow {
                rowSpacing: layout.spacing
                width: parent.width
                Layout.preferredHeight: root.rowHeight
                model: root.rowModels[index]
                isInEditorMode: root.isInEditorMode
                onBindingRequested: root.bindingRequested(quakeKey)
                onUnbindingRequested: root.unbindingRequested(quakeKey)
                onKeySelected: root.keySelected(quakeKey)
            }
        }
    }
}