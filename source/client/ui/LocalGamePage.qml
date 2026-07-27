import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    AppearDisappearHelper {
        id: appearDisappearHelper
        targets: [headerLabel, titleLabel, summaryLabel, pageIndicator]
    }
    StackView.onStatusChanged: appearDisappearHelper.shrinkAndHideIfDeactivating(StackView.status)

    UIHeaderLabel {
        id: headerLabel
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter
        baseText: "Local game"
    }

    UILabel {
        id: titleLabel
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: headerLabel.bottom
        anchors.topMargin: UI.titleLabelTopMargin
        height: UI.titleLabelHeight
        horizontalAlignment: Qt.AlignHCenter
        font.weight: Font.Medium
        text: "Step" + (stackView.currentItem.subpageIndex + 1) + "/3 - " + stackView.currentItem.subpageTitle
    }

    UILabel {
        id: summaryLabel
        anchors.top: titleLabel.bottom
        anchors.topMargin: UI.descLabelTopMargin
        anchors.horizontalCenter: parent.horizontalCenter
        horizontalAlignment: Qt.AlignHCenter
        width: parent.width
        height: UI.descLabelHeight
        maximumLineCount: 1
        elide: Qt.ElideRight
        text: {
            if (root.selectedGametypeName) {
                if (root.selectedMapName) {
                    "You have selected map <b>" + root.selectedMapName +
                    "</b> for gametype <b>" + root.selectedGametypeName + "</b>"
                } else {
                    "You have selected the gametype <b>" + root.selectedGametypeName + "</b>"
                }
            } else {
                ""
            }
        }
    }

    // Selected gametype props
    property int selectedGametypeIndex: -1
    property var selectedGametypeTitle
    property var selectedGametypeName
    property var selectedGametypeDesc
    property var gametypeMapsList

    // Selected map props
    property int selectedMapIndex: -1
    property var selectedMapName
    property var selectedMapTitle
    property var selectedMapMinPlayers
    property var selectedMapMaxPlayers

    SwipeLikeStackView {
        id: stackView
        clip: true
        width: 0.75 * parent.width
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.top: summaryLabel.bottom
        anchors.topMargin: 32
        anchors.bottom: buttonsBar.top
        anchors.bottomMargin: 32
        initialItem: gametypeComponent
    }

    Component {
        id: gametypeComponent
        LocalGameDetailArrangement {
            id: gametypePage

            desiredWidth: stackView.width
            desiredHeight: stackView.height

            readonly property int subpageIndex: 0
            readonly property string subpageTitle: "Select the gametype"

            readonly property bool prevButtonVisible: false
            readonly property bool nextButtonVisible: detailed

            function goNext() {
                console.assert(root.selectedGametypeIndex >= 0)
                stackView.switchRightTo(mapComponent)
            }

            function handleBackKey() {
                if (detailed) {
                    clearGlobalSelectedProps()
                    gametypePage.selectedIndex = -1
                    gametypePage.detailed      = false
                    return true
                }
                return false
            }

            function clearGlobalSelectedProps() {
                root.selectedGametypeIndex = -1
                root.selectedGametypeTitle = undefined
                root.selectedGametypeName  = undefined
                root.selectedGametypeDesc  = undefined
                root.gametypeMapsList      = undefined
            }

            Component.onCompleted: {
                // Restore detailed state if we return from next pages
                if (root.selectedGametypeIndex >= 0) {
                    gametypePage.selectedIndex = root.selectedGametypeIndex
                    gametypePage.detailed = true
                }
            }

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
                        root.selectedGametypeTitle = title
                        root.selectedGametypeName  = name
                        root.selectedGametypeIndex = index
                        root.selectedGametypeDesc  = desc
                        root.gametypeMapsList      = maps
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
                    text: root.selectedGametypeTitle || ""
                }
                SimpleVideoDecoration {
                    Layout.preferredWidth: gametypePage.expectedDetailsWidth
                    Layout.preferredHeight: gametypePage.expectedDetailsWidth * (9 / 16.0)
                    filePath: "videos/gametypes/" + root.selectedGametypeName + ".mpeg"
                }
                UILabel {
                    Layout.preferredWidth: gametypePage.expectedDetailsWidth - 16
                    Layout.alignment: Qt.AlignHCenter
                    horizontalAlignment: Qt.AlignJustify
                    maximumLineCount: 99
                    wrapMode: Text.WordWrap
                    elide: Qt.ElideRight
                    text: root.selectedGametypeDesc || ""
                }
            }
        }
    }

    Component {
        id: mapComponent
        LocalGameDetailArrangement {
            id: mapPage

            desiredWidth: stackView.width
            desiredHeight: stackView.height

            readonly property int subpageIndex: 1
            readonly property string subpageTitle: "Select the map"

            readonly property bool prevButtonVisible: true
            readonly property bool nextButtonVisible: detailed

            function goPrev() {
                clearGlobalSelectedProps()
                // Supplying the correct "detailed" property helps to avoid running redundant transitions
                stackView.switchLeftTo(gametypeComponent, {"detailed" : root.selectedGametypeIndex >= 0 })
            }

            function goNext() {
                console.assert(root.selectedMapIndex >= 0)
                stackView.switchRightTo(rulesComponent)
            }

            function handleBackKey() {
                if (detailed) {
                    clearGlobalSelectedProps()
                    mapPage.selectedIndex = -1
                    mapPage.detailed      = false
                    return true
                }
                return false
            }

            function clearGlobalSelectedProps() {
                root.selectedMapIndex      = -1
                root.selectedMapName       = undefined
                root.selectedMapTitle      = undefined
                root.selectedMapMinPlayers = 0
                root.selectedMapMaxPlayers = 0
            }

            Component.onCompleted: {
                // Restore detailed state if we return from next pages
                if (root.selectedMapIndex >= 0) {
                    mapPage.selectedIndex = root.selectedMapIndex
                    mapPage.detailed      = true
                }
            }

            listComponent: ListView {
                interactive: false
                model: root.gametypeMapsList
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
                        root.selectedMapName       = modelData["name"]
                        root.selectedMapTitle      = modelData["title"]
                        root.selectedMapIndex      = index
                        root.selectedMapMinPlayers = modelData["minPlayers"]
                        root.selectedMapMaxPlayers = modelData["maxPlayers"]
                        mapPage.detailed           = true
                    }
                }
            }

            detailComponent: ColumnLayout {
                spacing: 8
                UILabel {
                    Layout.preferredWidth: mapPage.expectedDetailsWidth
                    horizontalAlignment: Qt.AlignHCenter
                    font.weight: Font.Medium
                    font.pointSize: 16
                    font.capitalization: Font.AllUppercase
                    font.letterSpacing: 2
                    text: root.selectedMapTitle || ""
                }
                UILabel {
                    Layout.preferredWidth: mapPage.expectedDetailsWidth
                    horizontalAlignment: Qt.AlignHCenter
                    font.weight: Font.Medium
                    font.letterSpacing: 1
                    text: root.selectedMapName || ""
                }
                SimpleVideoDecoration {
                    Layout.preferredWidth: mapPage.expectedDetailsWidth
                    Layout.preferredHeight: mapPage.expectedDetailsWidth * (9 / 16.0)
                    filePath: "videos/maps/" + root.selectedMapName + ".mpeg"
                }
                UILabel {
                    Layout.fillWidth: true
                    visible: !!(root.selectedMapMinPlayers || root.selectedMapMaxPlayers)
                    horizontalAlignment: Qt.AlignHCenter
                    font.letterSpacing: 1
                    font.weight: Font.Medium
                    text: {
                        if (root.selectedMapMinPlayers != root.selectedMapMaxPlayers) {
                            "Optimal for <b>" + root.selectedMapMinPlayers + "-" + root.selectedMapMaxPlayers + "</b> players"
                        } else {
                            "Optimal for <b>" + root.selectedMapMaxPlayers + "</b> players"
                        }
                    }
                }
            }
        }
    }

    Component {
        id: rulesComponent
        Item {
            id: rulesPage
            readonly property int subpageIndex: 2
            readonly property string subpageTitle: "Set rules"

            readonly property bool prevButtonVisible: true
            readonly property bool nextButtonVisible: true

            property bool isNumBotsDefined
            property bool isNumBotsFixed

            Component.onCompleted: {
                const botConfig      = UI.gametypesModel.getBotConfig(root.selectedGametypeIndex, root.selectedMapIndex)
                isNumBotsDefined     = !!(botConfig["defined"])
                isNumBotsFixed       = !!(botConfig["fixed"])
                numBotsSpinBox.value = botConfig["number"] || 0
            }

            function goPrev() {
                // Supplying the correct "detailed" property helps to avoid running redundant transitions
                stackView.switchLeftTo(mapComponent, {"detailed" : root.selectedMapIndex >= 0})
            }

            function goNext() {
                let flags = 0
                if (instaCheckBox.checked) {
                    flags |= UISystem.LocalServerInsta
                }
                if (publicCheckBox.checked) {
                    flags |= UISystem.LocalServerPublic
                }
                const numBots    = numBotsSpinBox.value
                const skillLevel = skillLevelComboBox.currentIndex
                UI.ui.launchLocalServer(root.selectedGametypeName, root.selectedMapName, flags, numBots, skillLevel)
            }

            function handleBackKey() {
                return false
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
                    }
                }

                SettingsRow {
                    text: "Public (visible in LAN)"
                    UICheckBox {
                        id: publicCheckBox
                        Material.theme: checked ? Material.Light : Material.Dark
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
                    }
                }

                SettingsRow {
                    visible: rulesPage.isNumBotsDefined
                    text: "Bot skill"
                    AutoFittingComboBox {
                        id: skillLevelComboBox
                        model: ["Easy", "Medium", "Hard"]
                        Component.onCompleted: currentIndex = 1
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
        anchors.centerIn: buttonsBar
        count: 3
        currentIndex: stackView.currentItem.subpageIndex
        interactive: false
    }

    UIBackOrNextBar {
        id: buttonsBar
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: UI.acceptRejectRowBottomMargin
        backButtonVisible: stackView.currentItem.prevButtonVisible
        onBackButtonClicked: goPrev()
        nextButtonText: stackView.currentItem.subpageIndex === 2 ? "start" : "next"
        nextButtonVisible: stackView.currentItem.nextButtonVisible
        onNextButtonClicked: goNext()
    }

    function handleBackEvent(event) {
        if (stackView.currentItem.handleBackKey()) {
            UI.ui.playBackSound()
            event.accepted = true
            return true
        }
        if (stackView.currentItem.prevButtonVisible) {
            goPrev()
            event.accepted = true
            return true
        }
        return false
    }

    function handleCycleList(event, step) {
        const index = stackView.currentItem.subpageIndex
        // TODO: Fully delegate to
        if (index === 0 && stackView.currentItem.detailed) {
            stackView.currentItem.selectPrevOrNext(step)
        } else if (index === 1 && stackView.currentItem.detailed) {
            stackView.currentItem.selectPrevOrNext(step)
        }
        // Always consider it handled
        event.accepted = true
        return true
    }

    function goPrev() {
        UI.ui.playBackSound()
        stackView.currentItem.goPrev()
    }

    function goNext() {
        UI.ui.playForwardSound()
        stackView.currentItem.goNext()
    }

    function handleKeyEvent(event) {
        const key = event.key
        if (key === Qt.Key_Escape || key == Qt.Key_Back) {
            return handleBackEvent(event)
        }
        const item = stackView.currentItem
        if (key === Qt.Key_Left && item.prevButtonVisible) {
            goPrev()
            event.accepted = true
            return
        }
        if (key === Qt.Key_Right && item.nextButtonVisible) {
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