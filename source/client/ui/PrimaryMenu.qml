import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import QtGraphicalEffects 1.12
import net.warsow 2.6

Item {
	id: root

	property alias expansionFrac: decoratedLogo.expansionFrac

	readonly property bool tabVisible: expansionFrac < 1.0 && (UI.ui.isClientDisconnected || inGameMenuStackView.depth < 2)
	readonly property bool tabEnabled: expansionFrac <= 1.0
	readonly property real tabOpacity: 1.0 - expansionFrac

    readonly property bool canShowLoadouts: UI.gametypeOptionsModel.available && UI.hudCommonDataModel.realClientTeam !== HudDataModel.TeamSpectators

    TabBar {
        id: menuTabBar
        visible: tabVisible
        enabled: tabEnabled
        opacity: tabOpacity
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter
        width: 0.6 * mainMenuStackView.width
        height: UI.tabHeight
        background: null
        Component.onCompleted: {
            root.resetTabBarState(menuTabBar)
            menuTabBar.contentItem.highlight = null
        }
        UITabButton {
            text: "Play online"
            width: 0.25 * menuTabBar.width
            onClicked: root.selectMainMenuComponent(mainMenuPlayOnlineComponent)
        }
        UITabButton {
            text: "Local game"
            width: 0.25 * menuTabBar.width
            onClicked: root.selectMainMenuComponent(mainMenuLocalGameComponent)
        }
        UITabButton {
            text: "Demos"
            width: 0.25 * menuTabBar.width
            onClicked: root.selectMainMenuComponent(mainMenuDemosComponent)
        }
        UITabButton {
            text: "Settings"
            width: 0.25 * menuTabBar.width
            onClicked: root.selectMainMenuComponent(mainMenuSettingsComponent)
        }
    }

    TabBar {
        id: quitTabBar
        visible: tabVisible
        enabled: tabEnabled
        opacity: tabOpacity
        anchors.top: parent.top
        anchors.right: parent.right
        width: implicitWidth
        height: UI.tabHeight
        background: null
        Component.onCompleted: {
            root.resetTabBarState(quitTabBar)
            quitTabBar.contentItem.highlight = null
        }
        UITabButton {
            text: UI.ui.isClientDisconnected ? "Quit" : "Disconnect"
            width: (UI.ui.isClientDisconnected ? 0.10 : 0.15) * mainMenuStackView.width
            onClicked: {
                UI.ui.playForwardSound()
                if (UI.ui.isClientDisconnected) {
                    root.selectMainMenuComponent(mainMenuQuitComponent)
                } else {
                    if (UI.hudCommonDataModel.realClientTeam === HudDataModel.TeamSpectators || UI.hudCommonDataModel.isInWarmupState) {
                        UI.ui.disconnect()
                    } else {
                        inGameMenuStackView.push(inGameDisconnectConfirmationComponent)
                    }
                }
            }
        }
    }

    function selectMainMenuComponent(c) {
        decoratedLogo.toggleExpandedState()
        inGameMenuStackView.clear()
        // replace the entire stack
        mainMenuStackView.replace(null, c)
    }

    StackView {
		id: mainMenuStackView
		hoverEnabled: expansionFrac >= 1.0
		opacity: expansionFrac
		anchors.top: parent.top
		anchors.bottom: parent.bottom
		anchors.horizontalCenter: parent.horizontalCenter
		width: 1024 + 128
	}

	StackView {
	    id: inGameMenuStackView
	    // Assumes that we don't display the primary menu during connection
	    visible: !UI.ui.isClientDisconnected
	    hoverEnabled: visible
	    width: 600
        height: Math.max(600, 0.67 * parent.height)
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter

        Component.onCompleted: {
            if (!UI.ui.isClientDisconnected) {
                inGameMenuStackView.push(inGameGeneralComponent)
            }
        }
	}

	MainMenuDecoratedLogo {
	    // TODO: Load it only if needed, extract animation properties for proper expansion frac calculations
	    visible: UI.ui.isClientDisconnected
	    id: decoratedLogo
	    width: parent.width
	    // TODO: It's no longer just "tabOpacity"
	    opacity: tabOpacity
	    anchors.verticalCenter: parent.verticalCenter
	}

	function resetTabBarState(bar) {
	    bar.currentIndex = -1
	    for (let i = 0; i < bar.contentChildren.length; ++i) {
	        bar.contentChildren[i].checked = false
	    }
	}

    function resetStatesOfTabBars() {
        resetTabBarState(menuTabBar)
        resetTabBarState(quitTabBar)
    }

    function collapseMainMenu() {
        decoratedLogo.toggleExpandedState()
        resetStatesOfTabBars()
        mainMenuStackView.clear()
        root.forceActiveFocus()
        UI.ui.playBackSound()
    }

    onTabVisibleChanged: {
        if (tabVisible) {
            resetStatesOfTabBars()
        }
    }

    Connections {
        target: UI.ui
        onHudOccludersRetrievalRequested: {
            if (mainMenuStackView.depth > 0) {
                UI.ui.supplyHudOccluder(root)
            } else {
                UI.ui.supplyHudOccluder(inGameMenuStackView)
                UI.ui.supplyHudOccluder(menuTabBar)
                UI.ui.supplyHudOccluder(quitTabBar)
            }
        }
    }

	Keys.onPressed: {
	    const currentMainMenuItem = mainMenuStackView.currentItem
	    // TODO: Events propagation needs some attention and some work, e.g. setting the .accepted flag
	    // TODO: Check whether Keys.redirectTo is applicable
	    if (currentMainMenuItem) {
	        const handler = currentMainMenuItem["handleKeyEvent"]
	        if (handler && handler(event)) {
	            console.assert(event.accepted)
	            return
	        }
	    }
	    const currentInGameMenuItem = inGameMenuStackView.currentItem
	    if (currentInGameMenuItem) {
            const handler = currentInGameMenuItem["handleKeyEvent"]
            if (handler && handler(event)) {
                console.assert(event.accepted)
                return
            }
	    }
	    if (event.key === Qt.Key_Escape) {
	        if (currentMainMenuItem) {
	            collapseMainMenu()
	            if (!UI.ui.isClientDisconnected) {
	                inGameMenuStackView.push(inGameGeneralComponent)
	            }
	        } else {
	            if (currentInGameMenuItem) {
	                // Check if the current item can handle back navigation on its own
                    const handler = currentInGameMenuItem["handleKeyBack"]
                    if (handler && handler(event)) {
                        event.accepted = true
                        return
                    }
                    if (inGameMenuStackView.depth >= 2) {
                        UI.ui.playBackSound()
                        inGameMenuStackView.pop()
                        event.accepted = true
                        return
                    }
	            }
	            if (!UI.ui.isClientDisconnected) {
	                UI.ui.playBackSound()
	                UI.ui.returnFromPrimaryMenu()
	            }
	        }
	        event.accepted = true
	    }
	}

    Component {
        id: mainMenuPlayOnlineComponent
        PlayOnlinePage {
            onPlayingLocallySuggested: {
                UI.ui.playForwardSound()
                mainMenuStackView.replace(null, mainMenuLocalGameComponent)
            }
        }
    }

    Component {
        id: mainMenuLocalGameComponent
        LocalGamePage {}
    }

    Component {
        id: mainMenuSettingsComponent
        SettingsPage {}
    }

    Component {
        id: mainMenuDemosComponent
        DemosPage {}
    }

    Component {
        id: mainMenuQuitComponent
        QuitPage {
            backTrigger: () => collapseMainMenu()
        }
    }

    Component {
        id: inGameChatComponent
        InGameChatPage {}
    }

    Component {
        id: inGameCallvotesComponent
        InGameCallvotesPage {}
    }

    Component {
        id: inGameGeneralComponent
        Item {
            id: inGameGeneralPane
            readonly property real separatorWidth: 128
            readonly property real separatorHeight: 2
            readonly property real separatorMargins: 8
            readonly property real separatorRadius: 1
            ColumnLayout {
                id: buttonsLayout
                spacing: 20
                width: parent.width - 92 - 192
                anchors.centerIn: parent

                SlantedButton {
                    text: "Callvotes"
                    Layout.fillWidth: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        inGameMenuStackView.push(inGameCallvotesComponent)
                    }
                }
                SlantedButton {
                    text: "Chat"
                    Layout.fillWidth: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        inGameMenuStackView.push(inGameChatComponent)
                    }
                }

                Rectangle {
                    visible: canShowLoadouts
                    Layout.alignment: Qt.AlignHCenter
                    Layout.preferredWidth: inGameGeneralPane.separatorWidth
                    Layout.preferredHeight: inGameGeneralPane.separatorHeight
                    Layout.margins: inGameGeneralPane.separatorMargins
                    radius: inGameGeneralPane.separatorRadius
                }

                SlantedButton {
                    visible: canShowLoadouts
                    text: canShowLoadouts ? UI.gametypeOptionsModel.tabTitle : ""
                    Layout.fillWidth: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        inGameMenuStackView.push(inGameGametypeOptionsComponent)
                    }
                }

                Rectangle {
                    visible: readyButton.visible + joinButton.visible + switchTeamButton.visible +
                        queueButton.visible + spectateButton.visible > 1
                    Layout.alignment: Qt.AlignHCenter
                    Layout.preferredWidth: inGameGeneralPane.separatorWidth
                    Layout.preferredHeight: inGameGeneralPane.separatorHeight
                    Layout.margins: inGameGeneralPane.separatorMargins
                    radius: inGameGeneralPane.separatorRadius
                }

                SlantedButton {
                    id: readyButton
                    visible: UI.ui.canBeReady
                    highlightedWithAnim: visible && !UI.ui.isReady
                    text: UI.ui.isReady ? "Not ready" : "Ready"
                    Layout.fillWidth: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        if (UI.ui.isReady) {
                            UI.ui.setNotReady()
                        } else {
                            UI.ui.setReady()
                        }
                        UI.ui.returnFromPrimaryMenu()
                    }
                }
                SlantedButton {
                    id: joinButton
                    highlightedWithAnim: visible
                    visible: UI.ui.canJoin
                    text: "Join"
                    Layout.fillWidth: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        if (UI.hudCommonDataModel.hasTwoTeams && UI.ui.canJoinAlpha && UI.ui.canJoinBeta) {
                            inGameMenuStackView.push(inGameTeamSelectionComponent)
                        } else {
                            UI.ui.join()
                            inGameMenuStackView.push(inGameAwaitingJoinComponent)
                        }
                    }
                }
                SlantedButton {
                    id: switchTeamButton
                    visible: !UI.ui.canJoin && (UI.ui.canJoinAlpha !== UI.ui.canJoinBeta)
                    text: "Switch team"
                    Layout.fillWidth: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        if (UI.hudCommonDataModel.isInWarmupState) {
                            if (UI.ui.canJoinAlpha) {
                                UI.ui.joinAlpha()
                                inGameMenuStackView.push(inGameAwaitingSwitchTeamComponent, {"targetTeam" : HudDataModel.TeamAlpha})
                            } else {
                                UI.ui.joinBeta()
                                inGameMenuStackView.push(inGameAwaitingSwitchTeamComponent, {"targetTeam" : HudDataModel.TeamBeta})
                            }
                        } else {
                            inGameMenuStackView.push(inGameSwitchTeamConfirmationComponent)
                        }
                    }
                }
                SlantedButton {
                    id: queueButton
                    visible: UI.ui.canToggleChallengerStatus && UI.hudCommonDataModel.realClientTeam === HudDataModel.TeamSpectators
                    text: UI.ui.isInChallengersQueue ? "Leave the queue" : "Enter the queue"
                    Layout.fillWidth: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        if (UI.ui.isInChallengersQueue) {
                            UI.ui.leaveChallengersQueue()
                            UI.ui.returnFromPrimaryMenu()
                        } else {
                            UI.ui.enterChallengersQueue()
                            inGameMenuStackView.push(inGameAwaitingJoinQueueComponent)
                        }
                    }
                }
                SlantedButton {
                    id: spectateButton
                    visible: UI.ui.canSpectate
                    text: "Spectate"
                    Layout.fillWidth: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        if (UI.hudCommonDataModel.isInWarmupState) {
                            UI.ui.spectate()
                            UI.ui.returnFromPrimaryMenu()
                        } else {
                            inGameMenuStackView.push(inGameSpectateConfirmationComponent)
                        }
                    }
                }
            }
        }
    }

    Component {
        id: inGameGametypeOptionsComponent
        InGameGametypeOptionsPage {
            Connections {
                target: root
                onCanShowLoadoutsChanged: {
                    if (!root.canShowLoadouts) {
                        // Make copies as we can't access these variables after pop()
                        // TODO: Move to the root component?
                        const stackView  = inGameMenuStackView
                        const component_ = inGameGeneralComponent
                        stackView.pop(null, StackView.Immediate)
                        stackView.replace(null, component_)
                    }
                }
            }
        }
    }

    Component {
        id: inGameAwaitingJoinComponent
        Item {
            // Zero by default. Feasible teams are non-zero.
            property int maybeTargetTeam
            ProgressBar {
                anchors.centerIn: parent
                indeterminate: true
                Material.accent: "white"
            }
            Connections {
                target: UI.hudCommonDataModel
                onRealClientTeamChanged: {
                    // TODO: Should we check for the actual team?
                    if (UI.hudCommonDataModel.realClientTeam !== HudDataModel.TeamSpectators) {
                        UI.ui.returnFromPrimaryMenu()
                    }
                }
                onIsInPostmatchStateChanged: {
                    if (UI.ui.isInPostmatchState) {
                        inGameMenuStackView.pop()
                    }
                }
            }
            Timer {
                running: true
                interval: 2000
                onTriggered: {
                    inGameMenuStackView.pop()
                    if (maybeTargetTeam === HudDataModel.TeamAlpha || maybeTargetTeam === HudDataModel.TeamBeta) {
                        let actualTeamName
                        if (maybeTargetTeam === HudDataModel.TeamAlpha) {
                            actualTeamName = UI.hudCommonDataModel.alphaName;
                        } else {
                            actualTeamName = UI.hudCommonDataModel.betaName;
                        }
                        inGameMenuStackView.push(inGameActionFailureComponent, {"message" : "Failed to join the <b>" + actualTeamName + "</b> team"})
                    } else {
                        inGameMenuStackView.push(inGameActionFailureComponent, {"message" : "Failed to join"})
                    }
                }
            }
            Timer {
                running: true
                repeat: true
                interval: 750
                onTriggered: {
                    if (maybeTargetTeam === HudDataModel.TeamAlpha) {
                        UI.ui.joinAlpha()
                    } else if (maybeTargetTeam === HudDataModel.TeamBeta) {
                        UI.ui.joinBeta()
                    } else {
                        UI.ui.join()
                    }
                }
            }
        }
    }

    Component {
        id: inGameTeamSelectionComponent
        Item {
            property bool connectionsEnabled: true
            ColumnLayout {
                anchors.centerIn: parent
                width: parent.width
                spacing: 24
                UILabel {
                    Layout.fillWidth: true
                    horizontalAlignment: Qt.AlignHCenter
                    font.weight: Font.Medium
                    font.capitalization: Font.SmallCaps
                    font.pointSize: 18
                    font.letterSpacing: 1.25
                    text: "Select your team"
                }
                RowLayout {
                    spacing: 20
                    Layout.fillWidth: true
                    Layout.leftMargin: 20
                    Layout.rightMargin: 20
                    TeamSelectionTeamPane {
                        id: alphaTeamPane
                        model: UI.scoreboardAlphaModel
                        Layout.fillWidth: true
                        alignment: Qt.AlignRight
                        Layout.preferredHeight: Math.max(alphaTeamPane.implicitHeight, betaTeamPane.implicitHeight, 144 + 32)
                        color: UI.hudCommonDataModel.alphaColor
                    }
                    TeamSelectionTeamPane {
                        id: betaTeamPane
                        model: UI.scoreboardBetaModel
                        Layout.fillWidth: true
                        alignment: Qt.AlignLeft
                        Layout.preferredHeight: Math.max(alphaTeamPane.implicitHeight, betaTeamPane.implicitHeight, 144 + 32)
                        color: UI.hudCommonDataModel.betaColor
                    }
                }
                RowLayout {
                    Layout.alignment: Qt.AlignHCenter
                    spacing: 24
                    SlantedButton {
                        Layout.preferredWidth: UI.acceptOrRejectButtonWidth
                        displayIconPlaceholder: false
                        leftBodyPartSlantDegrees: -UI.buttonBodySlantDegrees
                        rightBodyPartSlantDegrees: -0.5 * UI.buttonBodySlantDegrees
                        textSlantDegrees: -0.3 * UI.buttonTextSlantDegrees
                        labelHorizontalCenterOffset: 0
                        Material.background: Qt.darker(UI.hudCommonDataModel.alphaColor, 2)
                        Material.accent: Qt.darker(UI.hudCommonDataModel.alphaColor, 1.2)
                        text: UI.hudCommonDataModel.alphaName
                        onClicked: {
                            connectionsEnabled = false
                            inGameMenuStackView.pop()
                            inGameMenuStackView.push(inGameAwaitingJoinComponent, {"maybeTargetTeam" : HudDataModel.TeamAlpha})
                            UI.ui.playForwardSound()
                            UI.ui.joinAlpha()
                        }
                    }
                    SlantedButton {
                        Layout.preferredWidth: UI.neutralCentralButtonWidth
                        displayIconPlaceholder: false
                        leftBodyPartSlantDegrees: -0.5 * UI.buttonBodySlantDegrees
                        rightBodyPartSlantDegrees: +0.5 * UI.buttonBodySlantDegrees
                        textSlantDegrees: 0
                        labelHorizontalCenterOffset: 0
                        Layout.alignment: Qt.AlignHCenter
                        text: "Any team"
                        onClicked: {
                            connectionsEnabled = false
                            inGameMenuStackView.pop()
                            inGameMenuStackView.push(inGameAwaitingJoinComponent)
                            UI.ui.playForwardSound()
                            UI.ui.join()
                        }
                    }
                    SlantedButton {
                        Layout.preferredWidth: UI.acceptOrRejectButtonWidth
                        displayIconPlaceholder: false
                        leftBodyPartSlantDegrees: 0.5 * UI.buttonBodySlantDegrees
                        rightBodyPartSlantDegrees: UI.buttonBodySlantDegrees
                        textSlantDegrees: +0.3 * UI.buttonTextSlantDegrees
                        labelHorizontalCenterOffset: 0
                        Material.background: Qt.darker(UI.hudCommonDataModel.betaColor, 2)
                        Material.accent: Qt.darker(UI.hudCommonDataModel.betaColor, 1.2)
                        text: UI.hudCommonDataModel.betaName
                        onClicked: {
                            connectionsEnabled = false
                            inGameMenuStackView.pop()
                            inGameMenuStackView.push(inGameAwaitingJoinComponent, {"maybeTargetTeam" : HudDataModel.TeamBeta})
                            UI.ui.playForwardSound()
                            UI.ui.joinBeta()
                        }
                    }
                }
            }
            Connections {
                target: UI.ui
                enabled: connectionsEnabled
                onCanJoinAlphaChanged: {
                    if (!UI.ui.canJoinAlpha) {
                        inGameMenuStackView.pop()
                    }
                }
                onCanJoinBetaChanged: {
                    if (!UI.ui.canJoinBeta) {
                        inGameMenuStackView.pop()
                    }
                }
            }
            Connections {
                target: UI.hudCommonDataModel
                enabled: connectionsEnabled
                onIsInPostmatchStateChanged: {
                    if (UI.hudCommonDataModel.isInPostmatchState) {
                        inGameMenuStackView.pop()
                    }
                }
            }
        }
    }

    Component {
        id: inGameSwitchTeamConfirmationComponent
        ConfirmationItem {
            titleText: "Switch team?"
            numButtons: 2
            buttonTexts: ["Switch", "Go back"]
            buttonFocusStatuses: [false, true]
            buttonEnabledStatuses: [true, true]
            onButtonClicked: {
                if (buttonIndex === 0) {
                    UI.ui.playForwardSound()
                    if (UI.ui.canJoinAlpha) {
                        inGameMenuStackView.pop()
                        inGameMenuStackView.push(inGameAwaitingSwitchTeamComponent, {"targetTeam" : HudDataModel.TeamAlpha})
                        UI.ui.joinAlpha()
                    } else {
                        inGameMenuStackView.pop()
                        inGameMenuStackView.push(inGameAwaitingSwitchTeamComponent, {"targetTeam" : HudDataModel.TeamBeta})
                        UI.ui.joinBeta()
                    }
                } else {
                    UI.ui.playBackSound()
                    inGameMenuStackView.pop()
                }
            }
            onButtonActiveFocusChanged: {
                const newStatuses = [...buttonFocusStatuses]
                newStatuses[buttonIndex] = buttonActiveFocus
                buttonFocusStatuses = newStatuses
            }
            Connections {
                target: UI.ui
                onCanJoinAlphaChanged: {
                    if (UI.ui.canJoinBeta) {
                        inGameMenuStackView.pop()
                    }
                }
                onCanJoinBetaChanged: {
                    if (UI.ui.canJoinAlpha) {
                        inGameMenuStackView.pop()
                    }
                }
            }
            Connections {
                target: UI.hudCommonDataModel
                onIsInPostmatchStateChanged: {
                    if (UI.hudCommonDataModel.isInPostmatchState) {
                        inGameMenuStackView.pop()
                    }
                }
            }
        }
    }

    Component {
        id: inGameAwaitingSwitchTeamComponent
        Item {
            property int targetTeam
            ProgressBar {
                anchors.centerIn: parent
                indeterminate: true
                Material.accent: "white"
            }
            Connections {
                target: UI.hudCommonDataModel
                onIsInPostmatchStateChanged: {
                    if (UI.hudCommonDataModel.isInPostmatchState) {
                        inGameMenuStackView.pop()
                    }
                }
                onRealClientTeamChanged: {
                    if (UI.hudCommonDataModel.realClientTeam === targetTeam) {
                        UI.ui.returnFromPrimaryMenu()
                    }
                }
            }
            Timer {
                running: true
                repeat: true
                interval: 750
                onTriggered: {
                    // TODO: Should there be a generic "join" method?
                    if (targetTeam === HudDataModel.TeamAlpha) {
                        UI.ui.joinAlpha()
                    } else {
                        UI.ui.joinBeta()
                    }
                }
            }
            Timer {
                running: true
                repeat: true
                interval: 2000
                onTriggered: {
                    inGameMenuStackView.pop()
                    inGameMenuStackView.push(inGameActionFailureComponent, {"message" : "Failed to switch team"})
                }
            }
        }
    }

    Component {
        id: inGameAwaitingJoinQueueComponent
        Item {
            ProgressBar {
                Material.accent: "white"
                anchors.centerIn: parent
                indeterminate: true
            }
            Connections {
                target: UI.ui
                onIsInChallengersQueueChanged: {
                    if (UI.ui.isInChallengersQueue) {
                        UI.ui.returnFromPrimaryMenu()
                    }
                }
            }
            Connections {
                target: UI.hudCommonDataModel
                onRealClientTeamChanged: {
                    // If in-game
                    if (UI.hudCommonDataModel.realClientTeam === HudDataModel.TeamSpectators) {
                        UI.ui.returnFromPrimaryMenu()
                    }
                }
            }
            // TODO: Can we really fail to join the queue?
            Timer {
                running: true
                repeat: true
                interval: 750
                onTriggered: UI.ui.joinChallengersQueue()
            }
            Timer {
                running: true
                interval: 2000
                onTriggered: inGameMenuStackView.push(inGameActionFailureComponent, {"message" : "Failed to join the challengers queue"})
            }
        }
    }

    Component {
        id: inGameSpectateConfirmationComponent
        ConfirmationItem {
            titleText: "Spectate?"
            numButtons: 2
            buttonTexts: ["Spectate", "Go back"]
            buttonFocusStatuses: [false, true]
            buttonEnabledStatuses: [true, true]
            onButtonClicked: {
                if (buttonIndex === 0) {
                    UI.ui.playForwardSound()
                    UI.ui.spectate()
                    UI.ui.returnFromPrimaryMenu()
                } else {
                    UI.ui.playBackSound()
                    inGameMenuStackView.pop()
                }
            }
            onButtonActiveFocusChanged: {
                const newStatuses = [...buttonFocusStatuses]
                newStatuses[buttonIndex] = buttonActiveFocus
                buttonFocusStatuses = newStatuses
            }
            Connections {
                target: UI.ui
                onCanSpectateChanged: {
                    if (!UI.ui.canSpectate) {
                        inGameMenuStackView.pop()
                    }
                }
            }
            Connections {
                target: UI.hudCommonDataModel
                onIsInPostmatchStateChanged: {
                    if (UI.hudCommonDataModel.isInPostmatchState) {
                        inGameMenuStackView.pop()
                    }
                }
            }
        }
    }

    Component {
        id: inGameDisconnectConfirmationComponent
        ConfirmationItem {
            titleText: "Disconnect?"
            numButtons: 2
            buttonTexts: ["Disconnect", "Go back"]
            buttonFocusStatuses: [false, true]
            buttonEnabledStatuses: [true, true]
            onButtonClicked: {
                if (buttonIndex === 0) {
                    UI.ui.playForwardSound()
                    UI.ui.disconnect()
                } else {
                    UI.ui.playBackSound()
                    inGameMenuStackView.pop()
                }
            }
            onButtonActiveFocusChanged: {
                const newStatuses = [...buttonFocusStatuses]
                newStatuses[buttonIndex] = buttonActiveFocus
                buttonFocusStatuses = newStatuses
            }
        }
    }

    Component {
        id: inGameActionFailureComponent
        Item {
            property alias message: confirmationItem.titleText
            ConfirmationItem {
                id: confirmationItem
                anchors.fill: parent
                numButtons: 1
                buttonTexts: ["OK"]
                buttonEnabledStatuses: [true]
                buttonFocusStatuses: [false]
                contentComponent: UILabel {
                    readonly property bool focusable: false
                    property int secondsLeft: 3
                    width: UI.desiredPopupContentWidth
                    horizontalAlignment: Qt.AlignHCenter
                    font.weight: Font.Medium
                    text: "Going back in " + secondsLeft + " seconds"
                    Timer {
                        running: true
                        repeat: true
                        interval: 1000
                        onTriggered: secondsLeft--
                    }
                }
                onButtonClicked: {
                    UI.ui.playBackSound()
                    inGameMenuStackView.pop()
                }
            }
            Timer {
                running: true
                interval: 3000
                onTriggered: {
                    inGameMenuStackView.pop()
                }
            }
        }
    }
}