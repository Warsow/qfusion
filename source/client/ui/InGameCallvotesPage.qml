import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    property int selectedVoteIndex
    property string selectedVoteName
    property string selectedVoteDesc
    property int selectedVoteFlags
    property int selectedVoteArgsHandle
    property int selectedVoteArgsKind
    property string selectedVoteCurrent
    property var selectedVoteChosen

    property var activeCallvotesModel: UI.ui.isOperator ? UI.operatorCallvotesModel : UI.regularCallvotesModel

    UIHeaderLabel {
        id: header
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter
        text: "Callvotes"
    }

    // TODO: Get rid of nested stack views?
    StackView {
        id: stackView
        width: root.width
        anchors.top: header.bottom
        anchors.bottom: parent.bottom
        anchors.bottomMargin: UI.acceptRejectRowHeight + UI.acceptRejectRowBottomMargin
        anchors.horizontalCenter: parent.horizontalCenter
    }

    UIBackOrNextBar {
        id: backOrNextBar

        visible: !backOrCallBar.visible

        anchors.bottom: parent.bottom
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottomMargin: UI.acceptRejectRowBottomMargin

        nextButtonVisible: false
        backButtonVisible: stackView.currentItem.canGoBack
        onBackButtonClicked: stackView.currentItem.goBack()
    }

    Item {
        id: backOrCallBar
        visible: stackView.currentItem.isFinalStage

        width: UI.acceptRejectRowWidth
        height: UI.acceptRejectRowHeight

        anchors.bottom: parent.bottom
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottomMargin: UI.acceptRejectRowBottomMargin

        SlantedLeftSecondaryButton {
            text: "back"
            anchors.left: parent.left
            anchors.verticalCenter: parent.verticalCenter
            onClicked: stackView.currentItem.goBack()
        }
        SlantedRightPrimaryButton {
            anchors.right: voteButton.left
            anchors.rightMargin: 16
            anchors.verticalCenter: parent.verticalCenter
            rightBodyPartSlantDegrees: voteButton.visible ? +0.3 * UI.buttonBodySlantDegrees : +UI.buttonBodySlantDegrees
            visible: UI.ui.isOperator && (selectedVoteFlags & CallvotesModel.Operator)
            enabled: stackView.currentItem.canCall
            highlighted: enabled && !voteButton.enabled
            text: "opcall"
            onClicked: startVote(stackView.currentItem.chosenValue, true)
        }
        SlantedRightPrimaryButton {
            id: voteButton
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter
            visible: selectedVoteFlags & CallvotesModel.Regular
            enabled: stackView.currentItem.canCall
            highlighted: enabled
            text: "vote"
            onClicked: startVote(stackView.currentItem.chosenValue, false)
        }
    }

    onActiveCallvotesModelChanged: {
        stackView.replace(null, groupSelectionComponent)
    }

    Connections {
        target: activeCallvotesModel
        onCurrentChanged: {
            if (index === selectedVoteIndex) {
                selectedVoteCurrent = value
            }
        }
    }

    Component {
        id: groupSelectionComponent

        Item {
            id: groupsPage
            readonly property bool isFinalStage: false
            readonly property bool canGoBack: false
            readonly property bool canCall: false

            ListView {
                anchors.centerIn: parent
                width: groupsPage.width
                height: Math.min(groupsPage.height, contentHeight)
                model: activeCallvotesModel ? activeCallvotesModel.getGroupsModel() : undefined
                spacing: 16

                delegate: SelectableLabel {
                    text: name
                    width: groupsPage.width
                    horizontalAlignment: Qt.AlignHCenter
                    onClicked: {
                        UI.ui.playForwardSound()
                        activeCallvotesModel.setGroupFilter(group)
                        stackView.replace(voteSelectionComponent)
                    }
                }
            }
        }
    }

    Component {
        id: voteSelectionComponent

        Item {
            id: votesPage
            readonly property bool isFinalStage: false
            readonly property bool canGoBack: true
            readonly property bool canCall: false
            function goBack() {
                UI.ui.playBackSound()
                stackView.replace(groupSelectionComponent)
            }

            ListView {
                anchors.centerIn: parent
                width: votesPage.width
                height: Math.min(votesPage.height, contentHeight)
                model: activeCallvotesModel
                spacing: 12

                delegate: SelectableLabel {
                    text: name
                    width: votesPage.width
                    horizontalAlignment: Qt.AlignHCenter
                    onClicked: {
                        UI.ui.playForwardSound()
                        selectedVoteIndex = index
                        selectedVoteName = name
                        selectedVoteDesc = desc
                        selectedVoteFlags = flags
                        selectedVoteArgsHandle = argsHandle
                        selectedVoteArgsKind = argsKind
                        selectedVoteCurrent = current
                        stackView.replace(argsSelectionComponent)
                    }
                }
            }
        }
    }

    Component {
        id: argsSelectionComponent

        ColumnLayout {
            readonly property bool isFinalStage: true
            readonly property bool canGoBack: true
            // TODO: s/canProceed/canCall
            readonly property bool canCall: argsSelectionLoader.item && argsSelectionLoader.item.canProceed
            readonly property var chosenValue: argsSelectionLoader.item ? argsSelectionLoader.item.chosenValue : null

            function goBack() {
                UI.ui.playBackSound()
                selectedVoteIndex = 0
                selectedVoteName = ""
                selectedVoteDesc = ""
                selectedVoteFlags = 0
                selectedVoteArgsHandle = 0
                selectedVoteArgsKind = 0
                selectedVoteCurrent = ""
                selectedVoteChosen = undefined
                stackView.replace(voteSelectionComponent)
            }

            Component.onCompleted: {
                if (selectedVoteArgsKind === CallvotesModel.Boolean) {
                    argsSelectionLoader.sourceComponent = booleanComponent
                } else if (selectedVoteArgsKind === CallvotesModel.Number) {
                    argsSelectionLoader.sourceComponent = numberComponent
                } else if (selectedVoteArgsKind === CallvotesModel.Player) {
                    argsSelectionLoader.sourceComponent = playerComponent
                } else if (selectedVoteArgsKind === CallvotesModel.Minutes) {
                    argsSelectionLoader.sourceComponent = minutesComponent
                } else if (selectedVoteArgsKind === CallvotesModel.Options) {
                    argsSelectionLoader.sourceComponent = optionsComponent
                } else if (selectedVoteArgsKind === CallvotesModel.MapList){
                    argsSelectionLoader.sourceComponent = mapListComponent
                } else {
                    argsSelectionLoader.sourceComponent = noArgsComponent
                }
            }

            Component {
                id: booleanComponent
                InGameCallvoteScalarSelector {
                    currentValue: selectedVoteCurrent
                    valueFormatter: v => v ? "YES" : "NO"
                    delegate: RowLayout {
                        spacing: 16
                        SelectableLabel {
                            Layout.preferredWidth: implicitWidth
                            Layout.preferredHeight: 32
                            enabled: currentValue != 1
                            selected: chosenValue == 1
                            text: "yes"
                            onClicked: {
                                UI.ui.playSwitchSound()
                                chosenValue = 1
                            }
                        }
                        SelectableLabel {
                            Layout.preferredWidth: implicitWidth
                            Layout.preferredHeight: 32
                            enabled: currentValue != 0
                            selected: chosenValue == 0
                            text: "no"
                            onClicked: {
                                UI.ui.playSwitchSound()
                                chosenValue = 0
                            }
                        }
                    }
                }
            }

            Component {
                id: numberComponent
                InGameCallvoteNumberSelector {
                    currentValue: selectedVoteCurrent
                }
            }

            Component {
                id: minutesComponent
                InGameCallvoteNumberSelector {
                    currentValue: selectedVoteCurrent
                    valueFormatter: v => v === 1 ? v + " minute" : v + " minutes"
                }
            }

            Component {
                id: playerComponent
                InGameCallvotePlayerSelector {
                    currentValue: selectedVoteCurrent
                }
            }

            Component {
                id: optionsComponent
                InGameCallvoteOptionSelector {
                    currentValue: selectedVoteCurrent
                    model: activeCallvotesModel.getOptionsList(selectedVoteArgsHandle)
                }
            }

            Component {
                id: mapListComponent
                InGameCallvoteMapSelector {
                    currentValue: selectedVoteCurrent
                    model: activeCallvotesModel.getOptionsList(selectedVoteArgsHandle)
                }
            }

            Component {
                id: noArgsComponent
                UILabel {
                    readonly property bool canProceed: true
                    readonly property string chosenValue: ""
                    readonly property string displayedString: ""
                    horizontalAlignment: Qt.AlignHCenter
                    verticalAlignment: Qt.AlignVCenter
                    text: "This vote has no arguments"
                }
            }

            UILabel {
                Layout.topMargin: UI.titleLabelTopMargin
                Layout.preferredWidth: root.width
                Layout.preferredHeight: UI.titleLabelHeight
                horizontalAlignment: Qt.AlignHCenter
                font.weight: Font.Medium
                text: "You have selected <b>" + selectedVoteName + "</b>"
            }
            UILabel {
                Layout.topMargin: UI.descLabelTopMargin
                Layout.preferredWidth: root.width
                Layout.preferredHeight: UI.descLabelHeight
                horizontalAlignment: Qt.AlignHCenter
                wrapMode: Text.WordWrap
                text: selectedVoteDesc
            }

            Loader {
                id: argsSelectionLoader
                Layout.topMargin: 16
                Layout.bottomMargin: 16
                Layout.preferredWidth: root.width
                Layout.fillHeight: true
            }

            UILabel {
                id: descLabel
                visible: selectedVoteArgsKind !== CallvotesModel.NoArgs
                Layout.preferredWidth: root.width
                Layout.bottomMargin: UI.tabHeight
                horizontalAlignment: Qt.AlignHCenter
                text: argsSelectionLoader.item.displayedString
            }
        }
    }

    function startVote(chosenValue, isOperatorCall) {
        UI.ui.callVote(selectedVoteName, chosenValue, isOperatorCall)
        UI.ui.playForwardSound()
        UI.ui.returnFromPrimaryMenu()
    }

    function handleKeyEvent(event) {
        if (event.key === Qt.Key_Escape || event.key === Qt.Key_Back) {
            const currentItem = stackView.currentItem
            if (currentItem.canGoBack) {
                currentItem.goBack()
                event.accepted = true
                return true
            }
        }
        return false
    }
}