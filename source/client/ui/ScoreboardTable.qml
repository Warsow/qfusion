import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

TableView {
    id: tableView
    columnSpacing: 0
    rowSpacing: 0
    reuseItems: false
    interactive: false

    contentHeight: rowHeight * rows
    implicitHeight: rowHeight * rows

    property color baseColor

    property color baseAlphaColor: "red"
    property color baseBetaColor: "green"

    property bool mixedTeamsMode: false

    property real baseCellWidth
    property real nameCellWidth
    property real clanCellWidth

    readonly property real rowHeight: 40

    function getCellColor(row, column, tableStyle) {
        let c = baseColor
        if (mixedTeamsMode) {
            c = UI.scoreboard.isMixedListRowAlpha(row) ? baseAlphaColor : baseBetaColor
        }
        switch (tableStyle) {
            case Scoreboard.Checkerboard: {
                if (row % 2) {
                    return column % 2 ? Qt.darker(c, 1.1) : c
                }
                return column % 2 ? c : Qt.lighter(c, 1.2)
            }
            case Scoreboard.RowStripes: {
                return row % 2 ? Qt.darker(c, 1.1) : Qt.lighter(c, 1.1)
            }
            case Scoreboard.ColumnStripes: {
                return column % 2 ? Qt.darker(c, 1.1) : Qt.lighter(c, 1.1)
            }
            case Scoreboard.Flat: {
                return c
            }
        }
    }

    delegate: Item {
        id: tableDelegate
        readonly property int kind: UI.scoreboard.getColumnKind(column)
        readonly property bool isColumnTextual: (kind === Scoreboard.Nickname) || (kind === Scoreboard.Clan)
        readonly property bool isColumnStatusOne: kind === Scoreboard.Status
        readonly property bool isDisplayingGlyph: (kind === Scoreboard.Glyph) || (isColumnStatusOne && value >= 32)
        readonly property bool shouldBeDisplayedAsIcon: (kind === Scoreboard.Icon) || (isColumnStatusOne && value < 32)
        readonly property bool isPovNickname: (kind === Scoreboard.Nickname)
            && UI.hudPovDataModel.hasActivePov && UI.hudPovDataModel.nickname === value
        readonly property real valueOpacity: isGhosting ? 0.5 : 1.0

        implicitWidth: Math.max(1,
            kind === Scoreboard.Nickname ? nameCellWidth :
                (kind === Scoreboard.Clan ? clanCellWidth : (isCompact ? 0.67 * baseCellWidth : baseCellWidth)))

        implicitHeight: rowHeight
        onImplicitWidthChanged: forceLayoutTimer.start()
        onImplicitHeightChanged: forceLayoutTimer.start()
        onHeightChanged: forceLayoutTimer.start()

        Rectangle {
            id: cellBackground
            anchors.fill: parent
            visible: !isColumnStatusOne
            opacity: 0.7
            color: isColumnStatusOne ? "transparent" : getCellColor(row, column, UI.scoreboard.tableStyle)
        }

        Label {
            visible: !shouldBeDisplayedAsIcon
            opacity: isColumnStatusOne ? 1.0 : valueOpacity
            anchors.fill: parent
            verticalAlignment: Qt.AlignVCenter
            horizontalAlignment: isColumnTextual ? Qt.AlignLeft : Qt.AlignHCenter
            padding: 4
            leftPadding: isPovNickname ? 12 : 6
            text: value
            textFormat: Text.StyledText
            font.family: ((kind !== Scoreboard.Glyph && kind !== Scoreboard.Status) || value < 256) ?
                UI.ui.regularFontFamily : UI.ui.symbolsFontFamily
            font.weight: Font.Bold
            font.pointSize: UI.scoreboardFontSize
            font.letterSpacing: UI.scoreboardLetterSpacing
            font.strikeout: isGhosting && isColumnTextual
            style: Text.Raised
        }

        Rectangle {
            visible: isPovNickname
            anchors.left: tableDelegate.left
            anchors.verticalCenter: parent.verticalCenter
            height: tableDelegate.height
            width: 7
            radius: 1
            color: Qt.rgba(1.0, 1.0, 1.0, 0.6)
        }

        Image {
            opacity: valueOpacity
            visible: shouldBeDisplayedAsIcon && value
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.verticalCenter: parent.verticalCenter
            mipmap: true
            width: 20
            height: 20
            source: UI.scoreboard.getImageAssetPath(value)
        }
    }

    onColumnsChanged: forceLayoutTimer.start()
    onRowsChanged: forceLayoutTimer.start()

    Timer {
        id: forceLayoutTimer
        interval: 1
        onTriggered: tableView.forceLayout()
    }
}