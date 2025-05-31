import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    StackView {
        id: stackView
        width: root.width - 16
        anchors.top: parent.top
        anchors.topMargin: 8
        anchors.bottom: parent.bottom
        anchors.horizontalCenter: parent.horizontalCenter
    }

    property int selectedVoteIndex
    property string selectedVoteName
    property string selectedVoteDesc
    property int selectedVoteFlags
    property int selectedVoteArgsHandle
    property int selectedVoteArgsKind
    property string selectedVoteCurrent
    property var selectedVoteChosen

    property var activeCallvotesModel: UI.ui.isOperator ? UI.operatorCallvotesModel : UI.regularCallvotesModel

    onActiveCallvotesModelChanged: {
        stackView.clear()
        stackView.replace(groupSelectionComponent)
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
            readonly property int stage: 1

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
            readonly property int stage: 2

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
            spacing: 20

            readonly property int stage: 3

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
                Layout.preferredWidth: root.width
                horizontalAlignment: Qt.AlignHCenter
                font.pointSize: 16
                font.capitalization: Font.AllUppercase
                font.weight: Font.Medium
                text: selectedVoteName
            }
            UILabel {
                Layout.preferredWidth: root.width
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
                horizontalAlignment: Qt.AlignHCenter
                text: argsSelectionLoader.item.displayedString
            }

            RowLayout {
                Layout.alignment: Qt.AlignHCenter
                spacing: UI.minAcceptRejectSpacing
                SlantedLeftSecondaryButton {
                    text: "back"
                    onClicked: {
                        UI.ui.playBackSound()
                        stackView.replace(voteSelectionComponent)
                    }
                }
                SlantedRightPrimaryButton {
                    visible: UI.ui.isOperator && (selectedVoteFlags & CallvotesModel.Operator)
                    enabled: argsSelectionLoader.item && argsSelectionLoader.item.canProceed
                    highlighted: enabled
                    text: "opcall"
                    onClicked: startVote(argsSelectionLoader.item.chosenValue, true)
                }
                SlantedRightPrimaryButton {
                    id: voteButton
                    visible: selectedVoteFlags & CallvotesModel.Regular
                    enabled: argsSelectionLoader.item && argsSelectionLoader.item.canProceed
                    highlighted: enabled
                    text: "vote"
                    onClicked: startVote(argsSelectionLoader.item.chosenValue, false)
                }
            }
        }
    }

    function startVote(chosenValue, isOperatorCall) {
        UI.ui.callVote(selectedVoteName, chosenValue, isOperatorCall)
        stackView.replace(voteSelectionComponent)
        UI.ui.playForwardSound()
        UI.ui.returnFromInGameMenu()
    }

    function handleKeyEvent(event) {
        if (event.key === Qt.Key_Escape || event.key === Qt.Key_Back) {
            const stage = stackView.currentItem.stage
            if (stage == 2 || stage == 3) {
                UI.ui.playBackSound()
                if (stage == 2) {
                    stackView.replace(groupSelectionComponent)
                } else {
                    stackView.replace(voteSelectionComponent)
                }
                event.accepted = true
                return true
            }
        }
        return false
    }
}