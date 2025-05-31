import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

SlantedButton {
    implicitWidth: UI.acceptOrRejectButtonWidth
    leftBodyPartSlantDegrees: +0.3 * UI.buttonBodySlantDegrees
    rightBodyPartSlantDegrees: +UI.buttonBodySlantDegrees
    textSlantDegrees: +0.3 * UI.buttonTextSlantDegrees
    labelHorizontalCenterOffset: 0
    font.weight: Font.Bold
}