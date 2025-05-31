import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Slider {
    onHoveredChanged: {
        if (hovered) {
            UI.ui.playHoverSound()
        }
    }
    onPressedChanged: {
        if (pressed) {
            UI.ui.playSwitchSound()
        }
    }
}
