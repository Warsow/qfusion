import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

TextField {
    hoverEnabled: true
    onHoveredChanged: {
        if (hovered) {
            UI.ui.playHoverSound()
        }
    }
    onActiveFocusChanged: {
        if (activeFocus) {
            UI.ui.playSwitchSound()
        }
    }
    onTextChanged: {
        if (activeFocus) {
            UI.ui.playSwitchSound()
        }
    }
}
