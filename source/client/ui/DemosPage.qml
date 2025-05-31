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

    readonly property real demoNameWeight: 1.1
    readonly property real serverNameWeight: 1.0

    readonly property real rowSpacing: selectedIndex >= 0 ? 22 : 16
    readonly property real headerAndListSpacing: rowSpacing
    readonly property real queryAndHeaderSpacing: 48

    readonly property real maxListViewHeight: (listView.count === 0) ? 0 :
        ((selectedIndex >= 0 ? UI.demoCompactRowHeight : UI.demoWideRowHeight) * listView.count +
            listView.spacing * (listView.count - 1))

    readonly property bool centered: maxListViewHeight < root.height + listHeader.height +
        queryAndHeaderSpacing + headerAndListSpacing + listView.anchors.bottomMargin

    property int selectedIndex: -1
    property var selectedGametype
    property var selectedDemoName
    property var selectedFileName
    property var selectedServerName
    property var selectedMapName
    property var selectedTimestamp
    property var selectedTags

    Component.onCompleted: UI.demosResolver.reload()

    states: [
        State {
            name: "centered"
            when: centered
            AnchorChanges {
                target: listHeader
                anchors.top: undefined
                anchors.bottom: listView.top
            }
            AnchorChanges {
                target: listView
                anchors.top: undefined
                anchors.bottom: undefined
                anchors.left: root.left
                anchors.verticalCenter: root.verticalCenter
            }
            PropertyChanges {
                target: listHeader
                anchors.topMargin: 0
                anchors.bottomMargin: headerAndListSpacing
            }
            PropertyChanges {
                target: listView
                height: contentHeight
                clip: false
                boundsBehavior: Flickable.StopAtBounds
            }
        },
        State {
            name: "fullHeight"
            when: !centered
            AnchorChanges {
                target: listHeader
                anchors.top: queryField.bottom
                anchors.bottom: undefined
            }
            AnchorChanges {
                target: listView
                anchors.top: listHeader.bottom
                anchors.bottom: root.bottom
                anchors.left: root.left
                anchors.verticalCenter: undefined
                anchors.horizontalCenter: undefined
            }
            PropertyChanges {
                target: listHeader
                anchors.topMargin: queryAndHeaderSpacing
                anchors.bottomMargin: 0
            }
            PropertyChanges {
                target: listView
                height: maxListViewHeight
                clip: true
                boundsBehavior: Flickable.OvershootBounds
            }
        }
    ]

    UITextField {
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

    UILabel {
        anchors.left: queryField.right
        anchors.verticalCenter: queryField.verticalCenter
        font.family: UI.ui.emojiFontFamily
        text: "\uD83D\uDD0D"
        visible: !queryField.text.length
    }

    UILabel {
        anchors.centerIn: parent
        text: "Nothing found"
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
        visible: selectedIndex < 0 && listView.count > 0
        // Keep it anchored to the parent item so supplied heading widths are consistent (TODO: This design is questionable)
        anchors.left: root.left
        anchors.right: root.right
        anchors.leftMargin: listView.anchors.leftMargin + rowSpacing / 3
        anchors.rightMargin: listView.anchors.leftMargin + rowSpacing / 3
        width: listView.width
        height: visible ? implicitHeight : 0
        spacing: rowSpacing / 2

        UILabel {
            id: demoHeading
            Layout.fillWidth: true
            Layout.preferredWidth: demoNameWeight
            horizontalAlignment: Qt.AlignLeft
            font.weight: Font.Bold
            text: "Name"
        }
        UILabel {
            id: serverHeading
            Layout.fillWidth: true
            Layout.preferredWidth: serverNameWeight
            horizontalAlignment: Qt.AlignHCenter
            font.weight: Font.Bold
            text: "Server"
        }
        UILabel {
            Layout.preferredWidth: mapColumnWidth
            horizontalAlignment: Qt.AlignHCenter
            font.weight: Font.Bold
            text: "Map"
        }
        UILabel {
            Layout.preferredWidth: gametypeColumnWidth
            horizontalAlignment: Qt.AlignHCenter
            font.weight: Font.Bold
            text: "Gametype"
        }
        UILabel {
            Layout.preferredWidth: timestampColumnWidth
            horizontalAlignment: Qt.AlignRight
            font.weight: Font.Bold
            text: "Timestamp"
        }
    }

    ListView {
        id: listView
        model: UI.demosModel
        anchors.topMargin: 24
        anchors.bottomMargin: 32
        anchors.leftMargin: 24
        width: selectedIndex >= 0 ? 0.5 * root.width : (root.width - 2 * anchors.leftMargin)
        spacing: selectedIndex >= 0 ? 1.75 * rowSpacing : rowSpacing

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
        id: detailsLoader
        active: selectedIndex >= 0
        anchors.left: listView.right
        anchors.leftMargin: 48
        anchors.verticalCenter: parent.verticalCenter
        sourceComponent: Item {
            width: root.width - listView.width - 4 * listView.anchors.leftMargin - anchors.leftMargin
            height: (9.0 / 16.0) * width

            Column {
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.bottom: parent.top
                anchors.bottomMargin: 24
                width: parent.width
                spacing: 8
                UILabel {
                    width: parent.width
                    horizontalAlignment: Qt.AlignHCenter
                    font.capitalization: Font.AllUppercase
                    font.weight: Font.Bold
                    elide: Text.ElideMiddle
                    text: selectedDemoName
                }
                UILabel {
                    width: parent.width
                    horizontalAlignment: Qt.AlignHCenter
                    elide: Text.ElideMiddle
                    text: selectedServerName
                }
                UILabel {
                    visible: selectedTags && selectedTags.length > 0
                    width: parent.width
                    horizontalAlignment: Qt.AlignHCenter
                    font.weight: Font.Normal
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

            UILabel {
                id: bottomLabel
                anchors.top: parent.bottom
                anchors.topMargin: 24
                anchors.left: parent.left
                anchors.right: parent.right
                horizontalAlignment: Qt.AlignHCenter
                elide: Text.ElideMiddle
                maximumLineCount: 1
                text: selectedMapName + " <b>-</b> " + selectedGametype + " <b>-</b> " + selectedTimestamp
            }

            SlantedButton {
                id: playButton
                anchors.top: bottomLabel.bottom
                anchors.topMargin: 24
                anchors.horizontalCenter: parent.horizontalCenter
                // Shorter than usual
                width: UI.acceptOrRejectButtonWidth
                leftBodyPartSlantDegrees: -0.5 * UI.buttonBodySlantDegrees
                rightBodyPartSlantDegrees: 0.5 * UI.buttonBodySlantDegrees
                textSlantDegrees: 0.0
                labelHorizontalCenterOffset: 0.0
                highlighted: true
                text: "Play"
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