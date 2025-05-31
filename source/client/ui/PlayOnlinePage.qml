import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    Component.onCompleted: {
        scanTimer.start()
        applyFilter()
    }

    Component.onDestruction: UI.ui.stopServerListUpdates()

    function applyFilter() {
        let flags = 0
        if (fullCheckBox.checked) {
            flags |= UISystem.ShowFullServers
        }
        if (emptyCheckBox.checked) {
            flags |= UISystem.ShowEmptyServers
        }
        UI.ui.startServerListUpdates(flags)
    }

    readonly property bool centered: tableView.contentHeight < root.height + optionsBar.height +
        optionsBar.topMargin + tableView.topMargin + tableView.bottomMargin

    states: [
        State {
            name: "centered"
            when: centered
            AnchorChanges {
                target: tableView
                anchors.top: undefined
                anchors.bottom: undefined
                anchors.horizontalCenter: root.horizontalCenter
                anchors.verticalCenter: root.verticalCenter
            }
            PropertyChanges {
                target: tableView
                height: contentHeight
                boundsBehavior: Flickable.StopAtBounds
            }
        },
        State {
            name: "fullHeight"
            when: !centered
            AnchorChanges {
                target: tableView
                anchors.top: optionsBar.bottom
                anchors.bottom: root.bottom
                anchors.horizontalCenter: root.horizontalCenter
                anchors.verticalCenter: undefined
            }
            PropertyChanges {
                target: tableView
                height: root.height - optionsBar.height - optionsBar.topMargin - tableView.topMargin - tableView.bottomMargin
                boundsBehavior: Flickable.OvershootBounds
            }
        }
    ]

    RowLayout {
        id: optionsBar
        anchors.top: parent.top
        anchors.topMargin: 16
        anchors.left: parent.left
        anchors.right: parent.right

        Item { Layout.fillWidth: true }

        UICheckBox {
            id: fullCheckBox
            Material.theme: checked ? Material.Light : Material.Dark
            text: "Show full"
            onCheckedChanged: applyFilter()
        }
        UICheckBox {
            id: emptyCheckBox
            Material.theme: checked ? Material.Light : Material.Dark
            text: "Show empty"
            onCheckedChanged: applyFilter()
        }
    }

    TableView {
        id: tableView
        visible: !scanTimer.running
        width: parent.width + columnSpacing
        anchors.topMargin: columnSpacing
        anchors.bottomMargin: columnSpacing
        columnSpacing: 28
        rowSpacing: 40
        interactive: true
        flickableDirection: Flickable.VerticalFlick
        clip: true

        onRowsChanged: {
            if (!rows) {
                scanTimer.start()
            }
        }

        model: UI.serverListModel

        delegate: ServerBrowserCard {
            implicitWidth: root.width / 2
            visible: typeof(model["serverName"]) !== "undefined"
            serverName: model["serverName"] || ""
            mapName: model["mapName"] || ""
            gametype: model["gametype"] || ""
            address: model["address"] || ""
            numPlayers: model["numPlayers"] || 0
            maxPlayers: model["maxPlayers"] || 0
            timeMinutes: model["timeMinutes"]
            timeSeconds: model["timeSeconds"]
            timeFlags: model["timeFlags"]
            alphaTeamName: model["alphaTeamName"]
            betaTeamName: model["betaTeamName"]
            alphaTeamScore: model["alphaTeamScore"]
            betaTeamScore: model["betaTeamScore"]
            alphaTeamList: model["alphaTeamList"]
            betaTeamList: model["betaTeamList"]
            playersTeamList: model["playersTeamList"]
            spectatorsList: model["spectatorsList"]

            onImplicitHeightChanged: forceLayoutTimer.start()
        }

        Timer {
            id: forceLayoutTimer
            interval: 1
            onTriggered: tableView.forceLayout()
        }

        Connections {
            target: UI.serverListModel
            onWasReset: tableView.forceLayout()
        }
    }

    Timer {
        id: scanTimer
        interval: 1750
    }

    Loader {
        anchors.centerIn: parent
        active: scanTimer.running
        sourceComponent: ColumnLayout {
            Component.onCompleted: opacity = 1.0
            opacity: 0.0
            Behavior on opacity { NumberAnimation { duration: 500 } }
            spacing: 24
            width: progressBar.implicitWidth
            ProgressBar {
                id: progressBar
                indeterminate: true
            }
            UILabel {
                Layout.fillWidth: true
                horizontalAlignment: Qt.AlignHCenter
                text: "Discovering servers\u2026"
            }
        }
    }

    Loader {
        anchors.centerIn: parent
        active: !scanTimer.running && !tableView.rows
        sourceComponent: ColumnLayout {
            Component.onCompleted: opacity = 1.0
            opacity: 0.0
            Behavior on opacity { NumberAnimation { duration: 500 } }
            spacing: 12
            width: 320
            UILabel {
                Layout.fillWidth: true
                horizontalAlignment: Qt.AlignHCenter
                text: "No servers found"
            }
            SlantedButton {
                Layout.preferredWidth: UI.neutralCentralButtonWidth
                Layout.alignment: Qt.AlignHCenter
                highlighted: true
                text: "Play offline"
                leftBodyPartSlantDegrees: -0.5 * UI.buttonBodySlantDegrees
                rightBodyPartSlantDegrees: 0.5 * UI.buttonBodySlantDegrees
                textSlantDegrees: 0
                labelHorizontalCenterOffset: 0
                onClicked: {
                    UI.ui.playForwardSound()
                    // TODO: This should be less hacky
                    primaryMenu.activePageTag = primaryMenu.pageLocalGame
                }
            }
        }
    }
}