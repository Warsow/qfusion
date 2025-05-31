import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    UILabel {
        id: titleLabel
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.topMargin: 32
        anchors.bottomMargin: 32
        horizontalAlignment: Qt.AlignHCenter
        font.weight: Font.Medium
        text: swipeView.currentItem.subpageTitle
    }

    UILabel {
        id: summaryLabel
        anchors.top: titleLabel.bottom
        anchors.topMargin: 20
        anchors.horizontalCenter: parent.horizontalCenter
        horizontalAlignment: Qt.AlignHCenter
        width: parent.width
        maximumLineCount: 1
        elide: Qt.ElideRight
        text: {
            if (swipeView.currentIndex === 2) {
                "You have selected map <b>" + selectedMapName +
                "</b> for gametype <b>" + selectedGametypeName + "</b>"
            } else if (swipeView.currentIndex === 1) {
                "You have selected the gametype <b>" + selectedGametypeName + "</b>"
            } else {
                ""
            }
        }
    }

    property var selectedGametypeTitle
    property var selectedGametypeName
    property var selectedGametypeIndex
    property var selectedGametypeDesc
    property var gametypeMapsList
    property var selectedMapName
    property var selectedMapTitle
    property var selectedMapMinPlayers
    property var selectedMapMaxPlayers
    property var selectedMapIndex
    property int selectedNumBots
    property int selectedSkillLevel
    property bool selectedInsta
    property bool selectedPublic

    SwipeView {
        id: swipeView
        clip: true
        interactive: false
        width: 0.75 * parent.width
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.top: summaryLabel.bottom
        anchors.topMargin: 32
        anchors.bottom: buttonsBar.top
        anchors.bottomMargin: 32

        LocalGameDetailArrangement {
            id: gametypePage
            readonly property string subpageTitle: "Step <b>1/3</b> - Select a gametype"
            readonly property bool canGoPrev: false
            readonly property bool canGoNext: detailed

            desiredWidth: swipeView.width
            desiredHeight: swipeView.height

            listComponent: ListView {
                id: gametypesListView
                model: UI.gametypesModel
                interactive: false
                delegate: LocalGameListDelegate {
                    detailed: gametypePage.detailed
                    width: gametypePage.expectedListItemWidth
                    text: title
                    selected: index === gametypePage.selectedIndex
                    onClicked: {
                        if (gametypePage.selectedIndex >= 0) {
                            UI.ui.playSwitchSound()
                        } else {
                            UI.ui.playForwardSound()
                        }
                        gametypePage.selectedIndex = index
                    }
                    // Handles external selectedIndex changes as well
                    onSelectedChanged: {
                        if (selected) {
                            selectProps()
                        }
                    }
                    function selectProps() {
                        selectedGametypeTitle = title
                        selectedGametypeName = name
                        selectedGametypeIndex = index
                        selectedGametypeDesc = desc
                        gametypeMapsList = maps
                        gametypePage.detailed = true
                    }
                }
            }

            detailComponent: ColumnLayout {
                spacing: 12
                UILabel {
                    Layout.preferredWidth: gametypePage.expectedDetailsWidth
                    horizontalAlignment: Qt.AlignHCenter
                    font.weight: Font.Medium
                    font.pointSize: 16
                    font.capitalization: Font.AllUppercase
                    font.letterSpacing: 2
                    text: selectedGametypeTitle || ""
                }
                SimpleVideoDecoration {
                    Layout.preferredWidth: gametypePage.expectedDetailsWidth
                    Layout.preferredHeight: gametypePage.expectedDetailsWidth * (9 / 16.0)
                    filePath: "videos/gametypes/" + selectedGametypeName + ".mpeg"
                }
                UILabel {
                    Layout.preferredWidth: gametypePage.expectedDetailsWidth - 16
                    Layout.alignment: Qt.AlignHCenter
                    horizontalAlignment: Qt.AlignJustify
                    maximumLineCount: 99
                    wrapMode: Text.WordWrap
                    elide: Qt.ElideRight
                    text: selectedGametypeDesc || ""
                }
            }
        }

        LocalGameDetailArrangement {
            id: mapPage
            readonly property string subpageTitle: "Step <b>2/3</b> - Select a map"
            readonly property bool canGoPrev: true
            readonly property bool canGoNext: detailed

            desiredWidth: swipeView.width
            desiredHeight: swipeView.height

            listComponent: ListView {
                interactive: false
                model: gametypeMapsList
                delegate: LocalGameListDelegate {
                    detailed: mapPage.detailed
                    width: mapPage.expectedListItemWidth
                    text: modelData["title"]
                    selected: index === mapPage.selectedIndex
                    onClicked: {
                        if (mapPage.selectedIndex >= 0) {
                            UI.ui.playSwitchSound()
                        } else {
                            UI.ui.playForwardSound()
                        }
                        mapPage.selectedIndex = index
                    }
                    // Handles external selectedIndex changes as well
                    onSelectedChanged: {
                        if (selected) {
                            selectProps()
                        }
                    }
                    function selectProps() {
                        selectedMapName = modelData["name"]
                        selectedMapTitle = modelData["title"]
                        selectedMapIndex = index
                        selectedMapMinPlayers = modelData["minPlayers"]
                        selectedMapMaxPlayers = modelData["maxPlayers"]
                        mapPage.detailed = true
                    }
                }
            }

            detailComponent: ColumnLayout {
                spacing: 8
                UILabel {
                    Layout.preferredWidth: gametypePage.expectedDetailsWidth
                    horizontalAlignment: Qt.AlignHCenter
                    font.weight: Font.Medium
                    font.pointSize: 16
                    font.capitalization: Font.AllUppercase
                    font.letterSpacing: 2
                    text: selectedMapTitle || ""
                }
                UILabel {
                    Layout.preferredWidth: gametypePage.expectedDetailsWidth
                    horizontalAlignment: Qt.AlignHCenter
                    font.weight: Font.Medium
                    font.letterSpacing: 1
                    text: selectedMapName || ""
                }
                SimpleVideoDecoration {
                    Layout.preferredWidth: gametypePage.expectedDetailsWidth
                    Layout.preferredHeight: gametypePage.expectedDetailsWidth * (9 / 16.0)
                    filePath: "videos/maps/" + selectedMapName + ".mpeg"
                }
                UILabel {
                    Layout.fillWidth: true
                    visible: !!(selectedMapMinPlayers || selectedMapMaxPlayers)
                    horizontalAlignment: Qt.AlignHCenter
                    font.letterSpacing: 1
                    font.weight: Font.Medium
                    text: {
                        if (selectedMapMinPlayers != selectedMapMaxPlayers) {
                            "Optimal for <b>" + selectedMapMinPlayers + "-" + selectedMapMaxPlayers + "</b> players"
                        } else {
                            "Optimal for <b>" + selectedMapMaxPlayers + "</b> players"
                        }
                    }
                }
            }
        }

        Item {
            id: rulesPage
            readonly property string subpageTitle: "Step <b>3/3</b> - Set rules"
            readonly property bool canGoPrev: true
            readonly property bool canGoNext: true

            property bool isNumBotsDefined
            property bool isNumBotsFixed
            property int numBots

            SwipeView.onIsCurrentItemChanged: {
                if (SwipeView.isCurrentItem) {
                    const botConfig      = UI.gametypesModel.getBotConfig(selectedGametypeIndex, selectedMapIndex)
                    isNumBotsDefined     = !!(botConfig["defined"])
                    isNumBotsFixed       = !!(botConfig["fixed"])
                    numBots              = botConfig["number"] || 0
                    numBotsSpinBox.value = numBots
                    selectedNumBots      = numBots
                }
            }

            ColumnLayout {
                anchors.centerIn: parent
                width: parent.width

                // OK, this is no longer a "Settings" row
                SettingsRow {
                    text: "Instagib"
                    UICheckBox {
                        id: instaCheckBox
                        Material.theme: checked ? Material.Light : Material.Dark
                        onCheckedChanged: selectedInsta = checked
                    }
                }

                SettingsRow {
                    text: "Public (visible in LAN)"
                    UICheckBox {
                        id: publicCheckBox
                        Material.theme: checked ? Material.Light : Material.Dark
                        onCheckedChanged: selectedPublic = checked
                    }
                }

                SettingsRow {
                    visible: rulesPage.isNumBotsDefined
                    text: "Number of bots"
                    UISpinBox {
                        id: numBotsSpinBox
                        visible: rulesPage.isNumBotsDefined
                        enabled: !rulesPage.isNumBotsFixed
                        from: 0; to: 9
                        textFromValue: v => (v !== 0) ? "" + v : "(none)"
                        onValueModified: {
                            if (selectedNumBots != value) {
                                selectedNumBots = value
                            }
                        }
                    }
                }

                SettingsRow {
                    visible: rulesPage.isNumBotsDefined
                    text: "Bot skill"
                    AutoFittingComboBox {
                        model: ["Easy", "Medium", "Hard"]
                        Component.onCompleted: {
                            currentIndex = 1
                        }
                        onCurrentIndexChanged: {
                            if (selectedSkillLevel != currentIndex) {
                                selectedSkillLevel = currentIndex
                            }
                        }
                    }
                }
            }
        }
    }

    Rectangle {
        anchors.centerIn: pageIndicator
        width: pageIndicator.width + 48
        height: pageIndicator.height + 20
        radius: 5
        color: Qt.rgba(0, 0, 0, 0.1)
    }

    PageIndicator {
        id: pageIndicator
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: buttonsBar.anchors.bottomMargin + 16
        count: 3
        currentIndex: swipeView.currentIndex
        interactive: false
    }

    RowLayout {
        id: buttonsBar
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 32
        width: UI.acceptRejectRowWidthFrac * parent.width

        SlantedLeftSecondaryButton {
            id: prevButton
            text: "back"
            visible: swipeView.currentItem.canGoPrev
            onClicked: {
                UI.ui.playBackSound()
                swipeView.currentIndex = swipeView.currentIndex - 1
            }
        }

        Item {
            Layout.preferredWidth: prevButton.Layout.preferredWidth
            visible: !prevButton.visible
        }

        Item { Layout.fillWidth: true }

        SlantedRightPrimaryButton {
            id: nextButton
            highlighted: true
            text: swipeView.currentIndex === 2 ? "start" : "next"
            visible: swipeView.currentItem.canGoNext
            onClicked: goNext()
        }

        Item {
            Layout.preferredWidth: prevButton.Layout.preferredWidth
            visible: !nextButton.visible
        }
    }

    function handleBackEvent(event) {
        const index = swipeView.currentIndex
        if (index === 0) {
            if (gametypePage.detailed) {
                gametypePage.detailed = false
                gametypePage.selectedIndex = -1
                event.accepted = true
                UI.ui.playBackSound()
                return true
            }
            return false
        }
        if (index === 1) {
            if (mapPage.detailed) {
                mapPage.detailed = false
                mapPage.selectedIndex = -1
            } else {
                swipeView.currentIndex = 0
            }
            UI.ui.playBackSound()
            event.accepted = true
            return true
        }
        UI.ui.playBackSound()
        swipeView.currentIndex = 1
        event.accepted = true
        return true
    }

    function handleCycleList(event, step) {
        const index = swipeView.currentIndex
        if (index === 0 && gametypePage.detailed) {
            gametypePage.selectPrevOrNext(step)
        }
        if (index === 1 && mapPage.detailed) {
            mapPage.selectPrevOrNext(step)
        }
        // Always consider it handled
        event.accepted = true
        return true
    }

    function goNext() {
        console.assert(swipeView.currentItem.canGoNext)
        UI.ui.playForwardSound()
        if (swipeView.currentIndex !== 2) {
            swipeView.currentIndex = swipeView.currentIndex + 1
        } else {
            let flags = 0
            if (selectedInsta) {
                flags |= UISystem.LocalServerInsta
            }
            if (selectedPublic) {
                flags |= UISystem.LocalServerPublic
            }
            UI.ui.launchLocalServer(selectedGametypeName, selectedMapName, flags, selectedNumBots, selectedSkillLevel)
        }
    }

    function handleKeyEvent(event) {
        const key = event.key
        if (key === Qt.Key_Escape || key == Qt.Key_Back) {
            return handleBackEvent(event)
        }
        if (key === Qt.Key_Left && swipeView.currentItem.canGoPrev) {
            return handleBackEvent(event)
        }
        if (key === Qt.Key_Right && swipeView.currentItem.canGoNext) {
            goNext()
            event.accepted = true
            return
        }
        if (key === Qt.Key_Up) {
            return handleCycleList(event, -1)
        }
        if (key === Qt.Key_Down) {
            return handleCycleList(event, +1)
        }
        // TODO: Accept ENTER key on the last page
        // TODO: Accept ENTER keys to continue
        // TODO: Accept TAB keys
        return false
    }
}