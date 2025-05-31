import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    visible: height > 0
    width: root.width
    anchors.top: matchTimeView.bottom
    implicitHeight: (!!alphaTeamList || !!betaTeamList) ? 36 : 0

    readonly property real scoreMargin: 25

    property var alphaTeamList
    property var betaTeamList
    property var alphaTeamScore
    property var betaTeamScore

    UILabel {
        visible: !!alphaTeamList
        anchors.left: parent.left
        anchors.right: alphaScoreLabel.left
        anchors.leftMargin: 24
        anchors.rightMargin: 12
        anchors.verticalCenter: parent.verticalCenter
        horizontalAlignment: Qt.AlignLeft
        textFormat: Text.StyledText
        text: alphaTeamName || ""
        maximumLineCount: 1
        elide: Text.ElideRight
        font.family: UI.ui.numbersFontFamily
        font.letterSpacing: 4
        font.weight: Font.Black
        font.pointSize: 16
    }

    UILabel {
        id: alphaScoreLabel
        width: implicitWidth
        anchors.right: parent.horizontalCenter
        anchors.rightMargin: scoreMargin
        anchors.verticalCenter: parent.verticalCenter
        font.family: UI.ui.headingFontFamily
        font.weight: Font.Black
        font.pointSize: 24
        text: typeof(alphaTeamList) !== "undefined" && typeof(alphaTeamScore) !== "undefined" ? alphaTeamScore : "-"
        transform: Scale { xScale: 1.0; yScale: 0.9 }
    }

    UILabel {
        id: betaScoreLabel
        width: implicitWidth
        anchors.left: parent.horizontalCenter
        anchors.leftMargin: scoreMargin - 8 // WTF?
        anchors.verticalCenter: parent.verticalCenter
        font.family: UI.ui.numbersFontFamily
        font.weight: Font.Black
        font.pointSize: 24
        text: typeof(betaTeamList) !== "undefined" && typeof(betaTeamScore) !== "undefined" ? betaTeamScore : "-"
        transform: Scale { xScale: 1.0; yScale: 0.9 }
    }

    UILabel {
        visible: !!betaTeamList
        anchors.left: betaScoreLabel.left
        anchors.right: parent.right
        anchors.leftMargin: 12 + 20 // WTF?
        anchors.rightMargin: 24 + 8 // WTF?
        anchors.verticalCenter: parent.verticalCenter
        horizontalAlignment: Qt.AlignRight
        textFormat: Text.StyledText
        text: betaTeamName || ""
        maximumLineCount: 1
        elide: Text.ElideLeft
        font.family: UI.ui.headingFontFamily
        font.letterSpacing: 4
        font.weight: Font.Black
        font.pointSize: 16
    }
}