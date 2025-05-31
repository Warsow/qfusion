import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

KeyboardKey {
    Connections {
        target: UI.keysAndBindings
        onMouseKeyBindingChanged: {
            if (changedQuakeKey === quakeKey) {
                group = UI.keysAndBindings.getMouseKeyBindingGroup(quakeKey)
            }
        }
    }
    Component.onCompleted: {
        group = UI.keysAndBindings.getMouseKeyBindingGroup(quakeKey)
    }
}