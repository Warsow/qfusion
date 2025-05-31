import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    readonly property bool canShowLoadouts: UI.gametypeOptionsModel.available && UI.hudCommonDataModel.realClientTeam !== HudDataModel.TeamSpectators

    CarouselTabBar {
        id: tabBar
        visible: stackView.depth < 2
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter
        width: canShowLoadouts ? Math.min(1.25 * mainPane.width, parent.width) : mainPane.width
        height: implicitHeight
        model: [
            {"text" : "General", "component": generalComponent},
            {"text" : "Chat", "component" : chatComponent},
            {"text" : "Callvotes", "component" : callvotesComponent},
        ]
        onCurrentIndexChanged: {
            // replace the entire stack
            stackView.replace(null, model[currentIndex]["component"])
        }
        Connections {
            target: UI.ui
            onHudOccludersRetrievalRequested: UI.ui.supplyHudOccluder(tabBar)
        }
    }

    Item {
        id: mainPane
        focus: true
        width: 600
        height: Math.max(600, 0.67 * parent.height)
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter

        StackView {
            id: stackView
            anchors.fill: parent
            anchors.margins: 16
            initialItem: generalComponent
            clip: true
        }

        Connections {
            target: UI.ui
            onHudOccludersRetrievalRequested: UI.ui.supplyHudOccluder(mainPane)
        }
    }

    Component {
        id: generalComponent
        Item {
            KeyNavigation.tab: mainMenuButton

            ColumnLayout {
                id: buttonsLayout
                spacing: 20
                width: parent.width - 64 - 192
                anchors.centerIn: parent

                SlantedButton {
                    id: mainMenuButton
                    text: "Main menu"
                    implicitHeight: UI.mainMenuButtonHeight
                    KeyNavigation.down: loadoutsButton
                    Layout.fillWidth: true
                    displayIconPlaceholder: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        UI.ui.showMainMenu()
                    }
                }
                SlantedButton {
                    id: loadoutsButton
                    implicitHeight: UI.mainMenuButtonHeight
                    KeyNavigation.up: mainMenuButton
                    KeyNavigation.down: readyButton
                    visible: canShowLoadouts
                    text: canShowLoadouts ? UI.gametypeOptionsModel.tabTitle : ""
                    Layout.fillWidth: true
                    displayIconPlaceholder: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        stackView.push(gametypeOptionsComponent)
                    }
                }
                SlantedButton {
                    id: readyButton
                    implicitHeight: UI.mainMenuButtonHeight
                    KeyNavigation.up: loadoutsButton
                    KeyNavigation.down: joinButton
                    visible: UI.ui.canBeReady
                    highlightedWithAnim: visible && !UI.ui.isReady
                    text: UI.ui.isReady ? "Not ready" : "Ready"
                    Layout.fillWidth: true
                    displayIconPlaceholder: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        if (UI.ui.isReady) {
                            UI.ui.setNotReady()
                        } else {
                            UI.ui.setReady()
                        }
                        UI.ui.returnFromInGameMenu()
                    }
                }
                SlantedButton {
                    id: joinButton
                    implicitHeight: UI.mainMenuButtonHeight
                    KeyNavigation.up: readyButton
                    KeyNavigation.down: switchTeamButton
                    highlightedWithAnim: visible
                    visible: UI.ui.canJoin
                    text: "Join"
                    Layout.fillWidth: true
                    displayIconPlaceholder: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        if (UI.hudCommonDataModel.hasTwoTeams && UI.ui.canJoinAlpha && UI.ui.canJoinBeta) {
                            stackView.push(teamSelectionComponent)
                        } else {
                            UI.ui.join()
                            stackView.push(awaitingJoinComponent)
                        }
                    }
                }
                SlantedButton {
                    id: switchTeamButton
                    implicitHeight: UI.mainMenuButtonHeight
                    KeyNavigation.up: joinButton
                    KeyNavigation.down: queueActionButton
                    visible: !UI.ui.canJoin && (UI.ui.canJoinAlpha !== UI.ui.canJoinBeta)
                    text: "Switch team"
                    Layout.fillWidth: true
                    displayIconPlaceholder: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        if (UI.hudCommonDataModel.isInWarmupState) {
                            if (UI.ui.canJoinAlpha) {
                                UI.ui.joinAlpha()
                                stackView.push(awaitingSwitchTeamComponent, {"targetTeam" : HudDataModel.TeamAlpha})
                            } else {
                                UI.ui.joinBeta()
                                stackView.push(awaitingSwitchTeamComponent, {"targetTeam" : HudDataModel.TeamBeta})
                            }
                        } else {
                            stackView.push(switchTeamConfirmationComponent)
                        }
                    }
                }
                SlantedButton {
                    id: queueActionButton
                    implicitHeight: UI.mainMenuButtonHeight
                    KeyNavigation.up: switchTeamButton
                    KeyNavigation.down: spectateButton
                    visible: UI.ui.canToggleChallengerStatus && UI.hudCommonDataModel.realClientTeam === HudDataModel.TeamSpectators
                    text: UI.ui.isInChallengersQueue ? "Leave the queue" : "Enter the queue"
                    Layout.fillWidth: true
                    displayIconPlaceholder: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        if (UI.ui.isInChallengersQueue) {
                            UI.ui.leaveChallengersQueue()
                            UI.ui.returnFromInGameMenu()
                        } else {
                            UI.ui.enterChallengersQueue()
                            stackView.push(awaitingJoinQueueComponent)
                        }
                    }
                }
                SlantedButton {
                    id: spectateButton
                    implicitHeight: UI.mainMenuButtonHeight
                    KeyNavigation.up: queueActionButton
                    KeyNavigation.down: disconnectButton
                    visible: UI.ui.canSpectate
                    text: "Spectate"
                    Layout.fillWidth: true
                    displayIconPlaceholder: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        if (UI.hudCommonDataModel.isInWarmupState) {
                            UI.ui.spectate()
                            UI.ui.returnFromInGameMenu()
                        } else {
                            stackView.push(spectateConfirmationComponent)
                        }
                    }
                }
                SlantedButton {
                    id: disconnectButton
                    implicitHeight: UI.mainMenuButtonHeight
                    KeyNavigation.up: spectateButton
                    text: "Disconnect"
                    Layout.fillWidth: true
                    displayIconPlaceholder: true
                    onClicked: {
                        UI.ui.playForwardSound()
                        if (UI.hudCommonDataModel.realClientTeam === HudDataModel.TeamSpectators || UI.hudCommonDataModel.isInWarmupState) {
                            UI.ui.disconnect()
                        } else {
                            stackView.push(disconnectConfirmationComponent)
                        }
                    }
                }
            }
        }
    }

    Component {
        id: chatComponent
        InGameChatPage {}
    }

    Component {
        id: callvotesComponent
        InGameCallvotesPage {}
    }

    Component {
        id: gametypeOptionsComponent
        InGameGametypeOptionsPage {
            Connections {
                target: root
                onCanShowLoadoutsChanged: {
                    if (!root.canShowLoadouts) {
                        // We have to destroy it forcefully so the model does not get accessed in illegal state during transition
                        // Save local references to vars on stack
                        // TODO: Can't we figure out something better
                        const sv = stackView
                        const tb = tabBar
                        sv.pop(null, StackView.Immediate)
                        // replace the entire stack
                        sv.replace(null, tb.model[0]["component"])
                    }
                }
            }
        }
    }

    Component {
        id: spectateConfirmationComponent
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
                    UI.ui.returnFromInGameMenu()
                } else {
                    UI.ui.playBackSound()
                    stackView.pop()
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
                        stackView.pop()
                    }
                }
            }
            Connections {
                target: UI.hudCommonDataModel
                onIsInPostmatchStateChanged: {
                    if (UI.hudCommonDataModel.isInPostmatchState) {
                        stackView.pop()
                    }
                }
            }
        }
    }

    Component {
        id: switchTeamConfirmationComponent
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
                        stackView.pop()
                        stackView.push(awaitingSwitchTeamComponent, {"targetTeam" : HudDataModel.TeamAlpha})
                        UI.ui.joinAlpha()
                    } else {
                        stackView.pop()
                        stackView.push(awaitingSwitchTeamComponent, {"targetTeam" : HudDataModel.TeamBeta})
                        UI.ui.joinBeta()
                    }
                } else {
                    UI.ui.playBackSound()
                    stackView.pop()
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
                        stackView.pop()
                    }
                }
                onCanJoinBetaChanged: {
                    if (UI.ui.canJoinAlpha) {
                        stackView.pop()
                    }
                }
            }
            Connections {
                target: UI.hudCommonDataModel
                onIsInPostmatchStateChanged: {
                    if (UI.hudCommonDataModel.isInPostmatchState) {
                        stackView.pop()
                    }
                }
            }
        }
    }

    Component {
        id: disconnectConfirmationComponent
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
                    stackView.pop()
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
        id: teamSelectionComponent
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
                            stackView.pop()
                            stackView.push(awaitingJoinComponent, {"maybeTargetTeam" : HudDataModel.TeamAlpha})
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
                            stackView.pop()
                            stackView.push(awaitingJoinComponent)
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
                            stackView.pop()
                            stackView.push(awaitingJoinComponent, {"maybeTargetTeam" : HudDataModel.TeamBeta})
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
                        stackView.pop()
                    }
                }
                onCanJoinBetaChanged: {
                    if (!UI.ui.canJoinBeta) {
                        stackView.pop()
                    }
                }
            }
            Connections {
                target: UI.hudCommonDataModel
                enabled: connectionsEnabled
                onIsInPostmatchStateChanged: {
                    if (UI.hudCommonDataModel.isInPostmatchState) {
                        stackView.pop()
                    }
                }
            }
        }
    }

    Component {
        id: awaitingJoinComponent
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
                        UI.ui.returnFromInGameMenu()
                    }
                }
                onIsInPostmatchStateChanged: {
                    if (UI.ui.isInPostmatchState) {
                        stackView.pop()
                    }
                }
            }
            Timer {
                running: true
                interval: 2000
                onTriggered: {
                    stackView.pop()
                    if (maybeTargetTeam === HudDataModel.TeamAlpha || maybeTargetTeam === HudDataModel.TeamBeta) {
                        let actualTeamName
                        if (maybeTargetTeam === HudDataModel.TeamAlpha) {
                            actualTeamName = UI.hudCommonDataModel.alphaName;
                        } else {
                            actualTeamName = UI.hudCommonDataModel.betaName;
                        }
                        stackView.push(actionFailureComponent, {"message" : "Failed to join the <b>" + actualTeamName + "</b> team"})
                    } else {
                        stackView.push(actionFailureComponent, {"message" : "Failed to join"})
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
        id: awaitingSwitchTeamComponent
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
                        stackView.pop()
                    }
                }
                onRealClientTeamChanged: {
                    if (UI.hudCommonDataModel.realClientTeam === targetTeam) {
                        UI.ui.returnFromInGameMenu()
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
                    stackView.pop()
                    stackView.push(actionFailureComponent, {"message" : "Failed to switch team"})
                }
            }
        }
    }

    Component {
        id: awaitingJoinQueueComponent
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
                        UI.ui.returnFromInGameMenu()
                    }
                }
            }
            Connections {
                target: UI.hudCommonDataModel
                onRealClientTeamChanged: {
                    // If in-game
                    if (UI.hudCommonDataModel.realClientTeam === HudDataModel.TeamSpectators) {
                        UI.ui.returnFromInGameMenu()
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
                onTriggered: stackView.push(actionFailureComponent, {"message" : "Failed to join the challengers queue"})
            }
        }
    }

    Component {
        id: actionFailureComponent
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
                    stackView.pop()
                }
            }
            Timer {
                running: true
                interval: 3000
                onTriggered: {
                    stackView.pop()
                }
            }
        }
    }

    Connections {
        target: stackView
        onCurrentItemChanged: {
            if (stackView.currentItem) {
                stackView.currentItem.forceActiveFocus()
            }
        }
    }

    Keys.onPressed: {
        if (visible) {
            const currentItem = stackView.currentItem
            if (currentItem) {
                // Check if the current item can handle the event on its own
                const handler = currentItem["handleKeyEvent"]
                if (handler && handler(event)) {
                    return
                }
                if (event.key === Qt.Key_Escape) {
                    event.accepted = true
                    // Check if the current item can handle back navigation on its own
                    const handler = currentItem["handleKeyBack"]
                    if (handler && handler(event)) {
                        return
                    }
                    UI.ui.playBackSound()
                    if (stackView.depth < 2) {
                        if (tabBar.currentIndex) {
                            tabBar.currentIndex = 0
                        } else {
                            UI.ui.returnFromInGameMenu()
                        }
                    } else {
                        stackView.pop()
                    }
                }
            }
        }
    }
}