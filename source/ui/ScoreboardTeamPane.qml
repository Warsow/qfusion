import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root

    property var model

    property bool displayHeader: true
    property real baseCellWidth
    property real nameCellWidth
    property real clanCellWidth
    property color baseColor

    property bool mixedTeamsMode: false
    property color baseAlphaColor
    property color baseBetaColor

    property int bufferIndex: 0

    implicitHeight: activeItem ? activeItem.implicitHeight + (header.status === Loader.Ready ? 40 : 0) : 0

    readonly property var activeItem: (front.status === Loader.Ready) ? front.item : back.item

    Loader {
        id: header
        width: root.width - baseCellWidth
        active: root.visible && displayHeader && activeItem && activeItem.rows
        sourceComponent: Item {
            implicitHeight: 40

            Rectangle {
                anchors.fill: parent
                color: "black"
                opacity: 0.3
            }

            Row {
                Repeater {
                    model: activeItem.columns - 1

                    delegate: Label {
                        id: title
                        readonly property int kind: UI.scoreboard.getColumnKind(index)
                        readonly property bool isTextual: kind == Scoreboard.Nickname || kind === Scoreboard.Clan
                        readonly property int span: UI.scoreboard.getTitleColumnSpan(index)
                        width:
                            (kind === Scoreboard.Nickname) ? nameCellWidth :
                                (kind === Scoreboard.Clan ? clanCellWidth :
                                    (span > 1 ? 0.67 : 1.0) * span * baseCellWidth)
                        leftPadding: isTextual ? 4 : 0
                        bottomPadding: 4
                        height: 40
                        verticalAlignment: Qt.AlignVCenter
                        horizontalAlignment: isTextual ? Qt.AlignLeft : Qt.AlignHCenter
                        text: UI.scoreboard.getColumnTitle(index)
                        font.family: UI.ui.headingFontFamily
                        font.weight: Font.Bold
                        font.capitalization: Font.AllUppercase
                        font.pointSize: UI.scoreboardFontSize
                        font.letterSpacing: UI.scoreboardLetterSpacing
                        style: Text.Raised
                    }
                }
            }
        }
    }

    // Hacks to force redrawing team tables.
	// (QML TableView does not destroy items immediately and they are visible through newly created transparent ones).
	// Neither forceLayout() nor reuseItems/onPooled/onReused work as expected.
	// TODO: Discover a better solution.

    Component {
        id: component
        ScoreboardTable {
            model: root.model
            baseColor: root.baseColor
            baseAlphaColor: root.baseAlphaColor
            baseBetaColor: root.baseBetaColor
            mixedTeamsMode: root.mixedTeamsMode
            baseCellWidth: root.baseCellWidth
            nameCellWidth: root.nameCellWidth
            clanCellWidth: root.clanCellWidth
        }
    }

    Loader {
        id: front
        anchors.top: parent.top
        anchors.topMargin: displayHeader ? 32 : 0
        width: root.width
        active: root.visible && bufferIndex === 0
        sourceComponent: component
    }

    Loader {
        id: back
        anchors.top: parent.top
        anchors.topMargin: displayHeader ? 32 : 0
        width: root.width
        active: root.visible && bufferIndex === 1
        sourceComponent: component
    }

    Connections {
        target: UI.scoreboard
        onTeamReset: {
            if (resetTeamTag === model.teamTag) {
                bufferIndex = (bufferIndex + 1) % 2
            }
        }
    }
}