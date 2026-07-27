import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Label {
    // TODO: Is there something better to do?
    property string baseText
    font.weight: Font.Black
    font.pointSize: UI.labelFontSize
    font.capitalization: Font.AllUppercase
    font.letterSpacing: 2.0
    height: UI.tabHeight
    verticalAlignment: Qt.AlignVCenter
    text: UI.headerTextPrefix + baseText + UI.headerTextSuffix
}