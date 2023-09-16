import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Rectangle {
    id: root
    color: UI.ui.colorWithAlpha(Material.background, UI.ui.fullscreenOverlayOpacity)

    readonly property bool canShowLoadouts: UI.gametypeOptionsModel.available && UI.hudDataModel.realClientTeam !== HudDataModel.TeamSpectators

    // Force redrawing stuff every frame
    ProgressBar {
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        indeterminate: true
        Material.accent: parent.Material.background
    }

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
        
        Component.onCompleted: UI.ui.registerHudOccluder(tabBar)
        Component.onDestruction: UI.ui.unregisterHudOccluder(tabBar)
        onWidthChanged: UI.ui.updateHudOccluder(tabBar)
        onHeightChanged: UI.ui.updateHudOccluder(tabBar)
        onXChanged: UI.ui.updateHudOccluder(tabBar)
        onYChanged: UI.ui.updateHudOccluder(tabBar)
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

        Component.onCompleted: UI.ui.registerHudOccluder(mainPane)
        Component.onDestruction: UI.ui.unregisterHudOccluder(mainPane)
        onWidthChanged: UI.ui.updateHudOccluder(mainPane)
        onHeightChanged: UI.ui.updateHudOccluder(mainPane)
        onXChanged: UI.ui.updateHudOccluder(mainPane)
        onYChanged: UI.ui.updateHudOccluder(mainPane)
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
                    KeyNavigation.down: loadoutsButton
                    Layout.fillWidth: true
                    onClicked: UI.ui.showMainMenu()
                }
                SlantedButton {
                    id: loadoutsButton
                    KeyNavigation.up: mainMenuButton
                    KeyNavigation.down: readyButton
                    visible: canShowLoadouts
                    text: canShowLoadouts ? UI.gametypeOptionsModel.tabTitle : ""
                    Layout.fillWidth: true
                    onClicked: stackView.push(gametypeOptionsComponent)
                }
                SlantedButton {
                    id: readyButton
                    KeyNavigation.up: loadoutsButton
                    KeyNavigation.down: joinButton
                    visible: UI.ui.canBeReady
                    highlightedWithAnim: visible && !UI.ui.isReady
                    text: UI.ui.isReady ? "Not ready" : "Ready"
                    Layout.fillWidth: true
                    onClicked: {
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
                    KeyNavigation.up: readyButton
                    KeyNavigation.down: switchTeamButton
                    highlightedWithAnim: visible
                    visible: UI.ui.canJoin
                    text: "Join"
                    Layout.fillWidth: true
                    onClicked: {
                        if (UI.hudDataModel.hasTwoTeams && UI.ui.canJoinAlpha && UI.ui.canJoinBeta) {
                            stackView.push(teamSelectionComponent)
                        } else {
                            UI.ui.join()
                            stackView.push(awaitingJoinComponent)
                        }
                    }
                }
                SlantedButton {
                    id: switchTeamButton
                    KeyNavigation.up: joinButton
                    KeyNavigation.down: queueActionButton
                    visible: !UI.ui.canJoin && (UI.ui.canJoinAlpha !== UI.ui.canJoinBeta)
                    text: "Switch team"
                    Layout.fillWidth: true
                    onClicked: {
                        if (UI.hudDataModel.isInWarmupState) {
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
                    KeyNavigation.up: switchTeamButton
                    KeyNavigation.down: spectateButton
                    visible: UI.ui.canToggleChallengerStatus && UI.hudDataModel.realClientTeam === HudDataModel.TeamSpectators
                    text: UI.ui.isInChallengersQueue ? "Leave the queue" : "Enter the queue"
                    Layout.fillWidth: true
                    onClicked: {
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
                    KeyNavigation.up: queueActionButton
                    KeyNavigation.down: disconnectButton
                    visible: UI.ui.canSpectate
                    text: "Spectate"
                    Layout.fillWidth: true
                    onClicked: {
                        if (UI.hudDataModel.isInWarmupState) {
                            UI.ui.spectate()
                            UI.ui.returnFromInGameMenu()
                        } else {
                            stackView.push(spectateConfirmationComponent)
                        }
                    }
                }
                SlantedButton {
                    id: disconnectButton
                    KeyNavigation.up: spectateButton
                    text: "Disconnect"
                    Layout.fillWidth: true
                    onClicked: {
                        if (UI.hudDataModel.realClientTeam === HudDataModel.TeamSpectators || UI.hudDataModel.isInWarmupState) {
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
                        stackView.pop()
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
                    UI.ui.spectate()
                    UI.ui.returnFromInGameMenu()
                } else {
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
                target: UI.hudDataModel
                onIsInPostmatchStateChanged: {
                    if (UI.hudDataModel.isInPostmatchState) {
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
                target: UI.hudDataModel
                onIsInPostmatchStateChanged: {
                    if (UI.hudDataModel.isInPostmatchState) {
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
                    UI.ui.disconnect()
                } else {
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
                Label {
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
                        color: UI.hudDataModel.alphaColor
                    }
                    TeamSelectionTeamPane {
                        id: betaTeamPane
                        model: UI.scoreboardBetaModel
                        Layout.fillWidth: true
                        alignment: Qt.AlignLeft
                        Layout.preferredHeight: Math.max(alphaTeamPane.implicitHeight, betaTeamPane.implicitHeight, 144 + 32)
                        color: UI.hudDataModel.betaColor
                    }
                }
                RowLayout {
                    Layout.alignment: Qt.AlignHCenter
                    spacing: 24
                    SlantedButton {
                        Layout.preferredWidth: 144
                        displayIconPlaceholder: false
                        labelHorizontalCenterOffset: -10
                        bodySlantDegrees: -9
                        textSlantDegrees: -4
                        Material.background: Qt.darker(UI.hudDataModel.alphaColor, 2)
                        Material.accent: Qt.darker(UI.hudDataModel.alphaColor, 1.2)
                        text: UI.hudDataModel.alphaName
                        onClicked: {
                            connectionsEnabled = false
                            stackView.pop()
                            stackView.push(awaitingJoinComponent, {"maybeTargetTeam" : HudDataModel.TeamAlpha})
                            UI.ui.joinAlpha()
                        }
                    }
                    SlantedButton {
                        Layout.preferredWidth: 144 + 16
                        displayIconPlaceholder: false
                        labelHorizontalCenterOffset: 0
                        bodySlantDegrees: 0
                        textSlantDegrees: 0
                        Layout.alignment: Qt.AlignHCenter
                        text: "Any team"
                        onClicked: {
                            connectionsEnabled = false
                            stackView.pop()
                            stackView.push(awaitingJoinComponent)
                            UI.ui.join()
                        }
                    }
                    SlantedButton {
                        Layout.preferredWidth: 144
                        displayIconPlaceholder: false
                        labelHorizontalCenterOffset: 0
                        bodySlantDegrees: +10
                        textSlantDegrees: +5
                        Material.background: Qt.darker(UI.hudDataModel.betaColor, 2)
                        Material.accent: Qt.darker(UI.hudDataModel.betaColor, 1.2)
                        text: UI.hudDataModel.betaName
                        onClicked: {
                            connectionsEnabled = false
                            stackView.pop()
                            stackView.push(awaitingJoinComponent, {"maybeTargetTeam" : HudDataModel.TeamBeta})
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
                target: UI.hudDataModel
                enabled: connectionsEnabled
                onIsInPostmatchStateChanged: {
                    if (UI.hudDataModel.isInPostmatchState) {
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
                target: UI.hudDataModel
                onRealClientTeamChanged: {
                    // TODO: Should we check for the actual team?
                    if (UI.hudDataModel.realClientTeam !== HudDataModel.TeamSpectators) {
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
                            actualTeamName = UI.hudDataModel.alphaName;
                        } else {
                            actualTeamName = UI.hudDataModel.betaName;
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
                target: UI.hudDataModel
                onIsInPostmatchStateChanged: {
                    if (UI.hudDataModel.isInPostmatchState) {
                        stackView.pop()
                    }
                }
                onRealClientTeamChanged: {
                    if (UI.hudDataModel.realClientTeam === targetTeam) {
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
                target: UI.hudDataModel
                onRealClientTeamChanged: {
                    // If in-game
                    if (UI.hudDataModel.realClientTeam === HudDataModel.TeamSpectators) {
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
                contentComponent: Label {
                    readonly property bool focusable: false
                    property int secondsLeft: 3
                    font.weight: Font.Medium
                    font.pointSize: 12
                    font.letterSpacing: 0.5
                    text: "Going back in " + secondsLeft + " seconds"
                    Timer {
                        running: true
                        repeat: true
                        interval: 1000
                        onTriggered: secondsLeft--
                    }
                }
                onButtonClicked: stackView.pop()
            }
            Timer {
                running: true
                interval: 3000
                onTriggered: stackView.pop()
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