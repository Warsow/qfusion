import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

CheckBox {
    Material.theme: checked ? Material.Light : Material.Dark
    Component.onCompleted: {
        contentItem.font.pointSize     = UI.labelFontSize
        contentItem.font.letterSpacing = UI.labelLetterSpacing
        contentItem.font.weight        = UI.labelFontWeight
        contentItem.color              = Material.foreground
    }
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
