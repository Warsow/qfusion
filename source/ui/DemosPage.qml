import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    property var pendingQuery

    readonly property real mapColumnWidth: 100
    readonly property real gametypeColumnWidth: 100
    readonly property real timestampColumnWidth: 144

    readonly property real demoNameWeight: 1.0
    readonly property real serverNameWeight: 1.0

    readonly property real leftListMargin: 100
    readonly property real rightListMargin: 100

    readonly property real rowSpacing: selectedIndex >= 0 ? 22 : 16

    property int selectedIndex: -1
    property var selectedGametype
    property var selectedDemoName
    property var selectedFileName
    property var selectedServerName
    property var selectedMapName
    property var selectedTimestamp
    property var selectedTags

    Component.onCompleted: UI.demosResolver.reload()

    WswTextField {
        id: queryField
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter
        horizontalAlignment: Qt.AlignHCenter
        maximumLength: 28
        width: 300
        Material.theme: Material.Dark
        Material.foreground: "white"
        Material.accent: "orange"

        onTextChanged: submitQuery(text)
        onEditingFinished: submitQuery(text)
    }

    Label {
        anchors.left: queryField.right
        anchors.verticalCenter: queryField.verticalCenter
        font.family: UI.ui.emojiFontFamily
        text: "\uD83D\uDD0D"
        visible: !queryField.text.length
    }

    Label {
        anchors.centerIn: parent
        text: "Nothing found"
        font.pointSize: 11
        visible: UI.demosResolver.isReady && !listView.count
    }

    function submitQuery(query) {
        selectedIndex = -1
        if (UI.demosResolver.isReady) {
            UI.demosResolver.query(query)
        } else {
            pendingQuery = query
        }
    }

    Connections {
        target: UI.demosResolver
        onIsReadyChanged: {
            if (UI.demosResolver.isReady && pendingQuery) {
                UI.demosResolver.query(pendingQuery)
                pendingQuery = undefined
            }
        }
    }

    RowLayout {
        id: listHeader
        visible: selectedIndex < 0
        anchors.top: queryField.bottom
        anchors.topMargin: 48
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.leftMargin: leftListMargin + rowSpacing / 2
        anchors.rightMargin: rightListMargin + rowSpacing / 2
        width: listView.width
        height: visible ? implicitHeight : 0
        spacing: rowSpacing / 2

        Label {
            id: demoHeading
            Layout.fillWidth: true
            Layout.preferredWidth: demoNameWeight
            horizontalAlignment: Qt.AlignLeft
            font.pointSize: 11
            font.weight: Font.Bold
            text: "Name"
        }
        Label {
            id: serverHeading
            Layout.fillWidth: true
            Layout.preferredWidth: serverNameWeight
            horizontalAlignment: Qt.AlignHCenter
            font.pointSize: 11
            font.weight: Font.Bold
            text: "Server"
        }
        Label {
            Layout.preferredWidth: mapColumnWidth
            horizontalAlignment: Qt.AlignHCenter
            font.pointSize: 11
            font.weight: Font.Bold
            text: "Map"
        }
        Label {
            Layout.preferredWidth: gametypeColumnWidth
            horizontalAlignment: Qt.AlignHCenter
            font.pointSize: 11
            font.weight: Font.Bold
            text: "Gametype"
        }
        Label {
            Layout.preferredWidth: timestampColumnWidth
            horizontalAlignment: Qt.AlignRight
            font.pointSize: 11
            font.weight: Font.Bold
            text: "Timestamp"
        }
    }

    ListView {
        id: listView
        model: UI.demosModel
        anchors.top: listHeader.bottom
        anchors.topMargin: 16
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 32
        anchors.left: parent.left
        anchors.leftMargin: leftListMargin
        width: (selectedIndex >= 0 ? 0.6 * parent.width : parent.width) - leftListMargin - rightListMargin
        spacing: selectedIndex >= 0 ? 1.5 * rowSpacing : rowSpacing
        clip: true

        delegate: DemosListDelegate {
            width: listView.width
            compact: selectedIndex >= 0
            selected: index === selectedIndex
            rowSpacing: root.rowSpacing

            demoName: model.demoName
            mapName: model.mapName
            serverName: model.serverName
            gametype: model.gametype
            timestamp: model.timestamp
            tags: model.tags

            demoColumnWidth: demoHeading.width
            serverColumnWidth: serverHeading.width
            mapColumnWidth: root.mapColumnWidth
            gametypeColumnWidth: root.gametypeColumnWidth
            timestampColumnWidth: root.timestampColumnWidth

            onContainsMouseChanged: {
                if (containsMouse) {
                    UI.ui.playHoverSound()
                }
            }

            onClicked: {
                if (selectedIndex >= 0) {
                    UI.ui.playSwitchSound()
                } else {
                    UI.ui.playForwardSound()
                }
                selectedDemoName = demoName
                selectedMapName = mapName
                selectedFileName = fileName
                selectedServerName = serverName
                selectedGametype = gametype
                selectedTimestamp = timestamp
                selectedTags = tags
                selectedIndex = index
                repositionListTimer.start()
            }
        }
    }

    Timer {
        id: repositionListTimer
        interval: 100
        onTriggered: {
            if (selectedIndex >= 0) {
                listView.positionViewAtIndex(selectedIndex, ListView.Visible)
            }
        }
    }

    Loader {
        active: selectedIndex >= 0
        anchors.left: listView.right
        anchors.leftMargin: 48
        anchors.verticalCenter: parent.verticalCenter
        sourceComponent: Item {
            width: root.width - listView.width - leftListMargin - rightListMargin
            height: 240

            Column {
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.bottom: parent.top
                anchors.bottomMargin: 24
                width: parent.width
                spacing: 8
                Label {
                    width: parent.width
                    horizontalAlignment: Qt.AlignHCenter
                    font.capitalization: Font.AllUppercase
                    font.pointSize: 12
                    font.weight: Font.Medium
                    font.letterSpacing: 1.0
                    elide: Text.ElideMiddle
                    text: selectedDemoName
                }
                Label {
                    width: parent.width
                    horizontalAlignment: Qt.AlignHCenter
                    font.pointSize: 12
                    font.weight: Font.Medium
                    font.letterSpacing: 0.5
                    elide: Text.ElideMiddle
                    text: selectedServerName
                }
                Label {
                    visible: selectedTags && selectedTags.length > 0
                    width: parent.width
                    horizontalAlignment: Qt.AlignHCenter
                    font.pointSize: 12
                    font.weight: Font.Normal
                    font.letterSpacing: 1.0
                    elide: Text.ElideMiddle
                    text: "<b>Tags</b>: " + selectedTags
                }
            }

            Rectangle {
                anchors.fill: parent
                opacity: 0.1
            }

            Image {
                anchors.fill: parent
                source: "image://wsw/levelshots/" + selectedMapName
                fillMode: Image.PreserveAspectCrop
                opacity: 0.5
            }

            Label {
                anchors.top: parent.bottom
                anchors.left: parent.left
                anchors.topMargin: 16
                font.pointSize: 11
                font.weight: Font.Medium
                text: selectedMapName + " " + selectedGametype
            }

            Label {
                id: timestampLabel
                anchors.top: parent.bottom
                anchors.right: parent.right
                anchors.topMargin: 16
                font.pointSize: 11
                font.weight: Font.Medium
                text: selectedTimestamp
            }

            Button {
                id: playButton
                anchors.top: timestampLabel.bottom
                anchors.topMargin: 4
                anchors.right: parent.right
                width: 56
                highlighted: true
                font.family: UI.ui.symbolsFontFamily
                text: "\u25B6"
                contentItem.anchors.verticalCenterOffset: +4

                onHoveredChanged: {
                    if (hovered) {
                        UI.ui.playHoverSound()
                    }
                }
                onClicked: {
                    selectedIndex = -1
                    UI.ui.playForwardSound()
                    UI.demoPlayer.play(selectedFileName)
                }
            }
        }
    }

    function handleKeyEvent(event) {
        if (event.key === Qt.Key_Escape) {
            if (selectedIndex >= 0) {
                selectedIndex = -1
                UI.ui.playBackSound()
                event.accepted = true
                return true
            }
        }
        return false
    }
}