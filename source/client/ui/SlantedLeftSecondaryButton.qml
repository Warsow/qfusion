import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

SlantedButton {
    implicitWidth: UI.acceptOrRejectButtonWidth
    leftBodyPartSlantDegrees: -UI.maxButtonBodySlantDegrees
    rightBodyPartSlantDegrees: -0.3 * UI.maxButtonBodySlantDegrees
    textSlantDegrees: -UI.maxButtonTextSlantDegrees
    labelHorizontalCenterOffset: 0
    Material.accent: Qt.lighter(Material.background, 2.0)
    font.weight: Font.Bold
}