import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    readonly property real baseCellWidth: 64
    readonly property real clanCellWidth: 96

    // TODO: Should it rather vary depending of number of columns?
    readonly property real tableWidth: 600

    property real nameCellWidth

    Component.onCompleted: updateCellParams()
    Connections {
        target: UI.scoreboard
        onSchemaChanged: updateCellParams()
    }

    function updateCellParams() {
        const columnsCount  = UI.scoreboard.getColumnsCount()
        let accumWidth      = clanCellWidth
        let cellsLeftInSpan = 0
        for (let i = 2; i < columnsCount; ++i) {
            if (cellsLeftInSpan > 0) {
                console.assert(UI.scoreboard.getTitleColumnSpan(i) <= 1)
                accumWidth += 0.67 * baseCellWidth
            } else {
                const span = UI.scoreboard.getTitleColumnSpan(i)
                if (span > 1) {
                    cellsLeftInSpan = span
                    accumWidth += 0.67 * baseCellWidth
                } else {
                    accumWidth += baseCellWidth
                }
            }
            // Harmless if negative
            cellsLeftInSpan--;
        }
        // TODO: Is this check redundant?
        if (nameCellWidth !== tableWidth - accumWidth) {
            nameCellWidth = tableWidth - accumWidth
        }
    }

    // A column layout could have been more apporpriate but it lacks hoirzontal center offset properties

    Loader {
        id: teamTablesLoader
        anchors.top: parent.top
        anchors.topMargin: 0.25 * root.height
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.horizontalCenterOffset: 0.5 * root.baseCellWidth
        sourceComponent: {
            if (!UI.hudCommonDataModel.hasTwoTeams) {
                playersDisplayComponent
            } else if (UI.scoreboard.layout === Scoreboard.Mixed) {
                mixedTeamsDisplayComponent
            } else if (UI.scoreboard.layout === Scoreboard.SideBySide && root.width > 2 * root.tableWidth) {
                sideBySideTeamsDisplayComponent
            } else {
                columnWiseTeamsDisplayComponent
            }
        }
    }

    Item {
        id: hudOccluder
        anchors.top: teamTablesLoader.top
        anchors.horizontalCenter: teamTablesLoader.horizontalCenter
        width: Math.max(teamTablesLoader.item.width, specsPane.width)
        height: teamTablesLoader.item.height + column.anchors.topMargin + column.height

        Connections {
            target: UI.ui
            onHudOccludersRetrievalRequested: UI.ui.supplyHudOccluder(hudOccluder)
        }
    }

    Column {
        id: column
        anchors.top: teamTablesLoader.bottom
        anchors.topMargin: 48
        anchors.horizontalCenter: parent.horizontalCenter
        spacing: 32

        ScoreboardSpecsPane {
            playersPerRow: 3
            playersInFirstRow: 2
            width: root.tableWidth - root.baseCellWidth
            height: implicitHeight
            model: UI.scoreboard.challengersModel
            title: "Challengers"
        }

        ScoreboardSpecsPane {
            id: specsPane
            playersPerRow: 3
            playersInFirstRow: 3
            width: root.tableWidth - root.baseCellWidth
            height: implicitHeight
            model: UI.scoreboard.specsModel
            title: "Spectators"
        }

        ScoreboardStatsPane {
            id: statsPane
            width: root.tableWidth - root.baseCellWidth
            height: implicitHeight
        }
    }

    Component {
        id: playersDisplayComponent

        ScoreboardTeamPane {
            width: root.tableWidth
            model: UI.scoreboardPlayersModel
            baseColor: Qt.lighter(Material.background)
            baseCellWidth: root.baseCellWidth
            nameCellWidth: root.nameCellWidth
            clanCellWidth: root.clanCellWidth
        }
    }

    Component {
        id: sideBySideTeamsDisplayComponent

        Row {
            ScoreboardTeamPane {
                width: root.tableWidth
                model: UI.scoreboardAlphaModel
                baseColor: Qt.darker(UI.hudCommonDataModel.alphaColor)
                baseCellWidth: root.baseCellWidth
                nameCellWidth: root.nameCellWidth
                clanCellWidth: root.clanCellWidth
            }
            ScoreboardTeamPane {
                width: root.tableWidth
                model: UI.scoreboardBetaModel
                baseColor: Qt.darker(UI.hudCommonDataModel.betaColor)
                baseCellWidth: root.baseCellWidth
                nameCellWidth: root.nameCellWidth
                clanCellWidth: root.clanCellWidth
            }
        }
    }

    Component {
        id: columnWiseTeamsDisplayComponent

        Column {
            spacing: 32
            ScoreboardTeamPane {
                width: root.tableWidth
                model: UI.scoreboardAlphaModel
                baseColor: Qt.darker(UI.hudCommonDataModel.alphaColor)
                baseCellWidth: root.baseCellWidth
                nameCellWidth: root.nameCellWidth
                clanCellWidth: root.clanCellWidth
            }
            ScoreboardTeamPane {
                width: root.tableWidth
                model: UI.scoreboardBetaModel
                displayHeader: false
                baseColor: Qt.darker(UI.hudCommonDataModel.betaColor)
                baseCellWidth: root.baseCellWidth
                nameCellWidth: root.nameCellWidth
                clanCellWidth: root.clanCellWidth
            }
        }
    }

    Component {
        id: mixedTeamsDisplayComponent

        ScoreboardTeamPane {
            width: root.tableWidth
            model: UI.scoreboardMixedModel
            mixedTeamsMode: true
            baseAlphaColor: Qt.darker(UI.hudCommonDataModel.alphaColor)
            baseBetaColor: Qt.darker(UI.hudCommonDataModel.betaColor)
            baseCellWidth: root.baseCellWidth
            nameCellWidth: root.nameCellWidth
            clanCellWidth: root.clanCellWidth
        }
    }
}