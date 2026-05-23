import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

TabButton {
    height: UI.tabHeight
    width: implicitWidth

    background: null
    font.weight: Font.Black
    font.pointSize: checked ? UI.labelFontSize + 2 : UI.labelFontSize
    Behavior on font.pointSize { SmoothedAnimation { duration: 250 } }
    font.letterSpacing: checked ? 2.0 : 1.25
    Behavior on font.letterSpacing { SmoothedAnimation { duration: 250 } }
}
