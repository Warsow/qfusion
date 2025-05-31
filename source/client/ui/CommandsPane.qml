import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

RowLayout {
    id: root
    spacing: 32

    signal bindingRequested(int command)
    signal bindingSelected(int command)

    property bool isInEditorMode
    property bool allowMultiBind

    readonly property color movementGroupColor: UI.keysAndBindings.colorForGroup(KeysAndBindings.MovementGroup)
    readonly property color weaponGroupColor: UI.keysAndBindings.colorForGroup(KeysAndBindings.WeaponGroup)
    readonly property color actionGroupColor: UI.keysAndBindings.colorForGroup(KeysAndBindings.ActionGroup)
    readonly property color respectGroupColor: UI.keysAndBindings.colorForGroup(KeysAndBindings.RespectGroup)

    ColumnLayout {
        width: movementColumn.width
        Layout.alignment: Qt.AlignTop
        spacing: 16

        UILabel {
            Layout.alignment: Qt.AlignHCenter
            text: "Movement"
            font.weight: Font.Medium
            font.capitalization: Font.AllUppercase
        }

        Rectangle {
            height: UI.boldLineHeight; width: movementColumn.width
            color: movementGroupColor
        }

        BindableCommandsColumn {
            id: movementColumn
            Layout.alignment: Qt.AlignTop
            model: UI.keysAndBindings.commandsMovementColumn
            highlightColor: movementGroupColor
            isInEditorMode: root.isInEditorMode
            allowMultiBind: root.allowMultiBind
            onBindingRequested: root.bindingRequested(command)
            onBindingSelected: root.bindingSelected(command)
        }
    }

    ColumnLayout {
        width: weaponColumnsRow.width
        Layout.alignment: Qt.AlignTop
        spacing: 16

        UILabel {
            Layout.alignment: Qt.AlignHCenter
            text: "Weapons"
            font.weight: Font.Medium
            font.capitalization: Font.AllUppercase
        }

        Rectangle {
            height: UI.boldLineHeight; width: weaponColumnsRow.width
            color: weaponGroupColor
        }

        RowLayout {
            id: weaponColumnsRow
            spacing: 40

            BindableCommandsColumn {
                Layout.alignment: Qt.AlignTop
                model: UI.keysAndBindings.commandsWeaponsColumn1
                highlightColor: weaponGroupColor
                isInEditorMode: root.isInEditorMode
                allowMultiBind: root.allowMultiBind
                onBindingRequested: root.bindingRequested(command)
                onBindingSelected: root.bindingSelected(command)
            }

            BindableCommandsColumn {
                Layout.alignment: Qt.AlignTop
                model: UI.keysAndBindings.commandsWeaponsColumn2
                highlightColor: weaponGroupColor
                isInEditorMode: root.isInEditorMode
                allowMultiBind: root.allowMultiBind
                onBindingRequested: root.bindingRequested(command)
                onBindingSelected: root.bindingSelected(command)
            }
        }
    }

    ColumnLayout {
        width: actionsColumn.width
        Layout.alignment: Qt.AlignTop
        spacing: 16

        UILabel {
            Layout.alignment: Qt.AlignHCenter
            text: "Actions"
            font.weight: Font.Medium
            font.capitalization: Font.AllUppercase
        }

        Rectangle {
            height: UI.boldLineHeight; width: actionsColumn.width
            color: actionGroupColor
        }

        BindableCommandsColumn {
            id: actionsColumn
            Layout.alignment: Qt.AlignTop
            model: UI.keysAndBindings.commandsActionsColumn
            highlightColor: actionGroupColor
            isInEditorMode: root.isInEditorMode
            allowMultiBind: root.allowMultiBind
            onBindingRequested: root.bindingRequested(command)
            onBindingSelected: root.bindingSelected(command)
        }
    }

    ColumnLayout {
        width: respectColumnsRow.width
        Layout.alignment: Qt.AlignTop
        spacing: 16

        UILabel {
            Layout.alignment: Qt.AlignHCenter
            text: "R&S Tokens"
            font.weight: Font.Medium
            font.capitalization: Font.AllUppercase
        }

        Rectangle {
            height: UI.boldLineHeight; width: respectColumnsRow.width
            color: respectGroupColor
        }

        RowLayout {
            id: respectColumnsRow
            spacing: 40

            BindableCommandsColumn {
                Layout.alignment: Qt.AlignTop
                model: UI.keysAndBindings.commandsRespectColumn1
                highlightColor: respectGroupColor
                isInEditorMode: root.isInEditorMode
                allowMultiBind: root.allowMultiBind
                onBindingRequested: root.bindingRequested(command)
                onBindingSelected: root.bindingSelected(command)
            }

            BindableCommandsColumn {
                Layout.alignment: Qt.AlignTop
                model: UI.keysAndBindings.commandsRespectColumn2
                highlightColor: respectGroupColor
                isInEditorMode: root.isInEditorMode
                allowMultiBind: root.allowMultiBind
                onBindingRequested: root.bindingRequested(command)
                onBindingSelected: root.bindingSelected(command)
            }
        }
    }
}