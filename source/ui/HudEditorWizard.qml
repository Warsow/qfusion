import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    signal exitRequested()

    readonly property real listItemWidth: 300

    UILabel {
        id: titleLabel
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.topMargin: 32
        anchors.bottomMargin: 32
        horizontalAlignment: Qt.AlignHCenter
        font.weight: Font.Medium
        text: "Step <b>" + (stackView.currentItem.stageIndex + 1) + "/4</b> - " + stackView.currentItem.subpageTitle
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
        text: stackView.currentItem.summary
    }

    StackView {
        id: stackView
        anchors.top: summaryLabel.bottom
        anchors.bottom: buttonsBar.top
        anchors.left: parent.left
        anchors.right: parent.right
        clip: true
        initialItem: flavorSelectionComponent
        function setComponent(c, props) { replace(c, props) }
    }

    Component {
        id: flavorSelectionComponent
        Item {
            id: flavorSelectionPage

            property var selectedHudEditorModel

            readonly property string subpageTitle: "Select a HUD flavor"
            readonly property string summary: {
                if (selectedHudEditorModel === UI.regularHudEditorModel) {
                    "You have selected editing primary HUD models"
                } else if (selectedHudEditorModel == UI.miniviewHudEditorModel) {
                    "You have selected editing miniview HUD models"
                } else {
                    ""
                }
            }

            readonly property bool canGoBack: false
            readonly property bool canGoNext: !!selectedHudEditorModel
            readonly property int stageIndex: 0

            width: root.width

            function handleNextRequest() {
                console.assert(selectedHudEditorModel)
                UI.ui.playForwardSound()
                stackView.setComponent(loadPageComponent, {"selectedHudEditorModel" : selectedHudEditorModel})
            }

            Column {
                anchors.centerIn: parent
                spacing: 12

                SelectableLabel {
                    width: listItemWidth
                    text: "Primary"
                    selected: selectedHudEditorModel === UI.regularHudEditorModel
                    onClicked: {
                        UI.ui.playSwitchSound()
                        selectedHudEditorModel = UI.regularHudEditorModel
                    }
                }
                SelectableLabel {
                    width: listItemWidth
                    text: "Miniview"
                    selected: selectedHudEditorModel === UI.miniviewHudEditorModel
                    onClicked: {
                        UI.ui.playSwitchSound()
                        selectedHudEditorModel = UI.miniviewHudEditorModel
                    }
                }
            }
        }
    }

    Component {
        id: loadPageComponent
        Item {
            id: loadPage

            property var selectedHudEditorModel
            property var selectedForLoadingFileName

            readonly property string subpageTitle: "Select a HUD file to load"
            readonly property string summary: {
                if (selectedForLoadingFileName) {
                    'You have selected loading of the <b><font color="yellow">' +
                        selectedForLoadingFileName + "</font></b> HUD"
                } else {
                    ''
                }
            }

            readonly property bool canGoBack: true
            readonly property bool canGoNext: !!selectedForLoadingFileName
            readonly property int stageIndex: 1

            property int selectedIndex: -1
            width: root.width

            Component.onCompleted: {
                console.assert(selectedHudEditorModel)
            }

            function handleBackRequest() {
                console.assert(selectedHudEditorModel)
                UI.ui.playBackSound()
                stackView.setComponent(flavorSelectionComponent)
            }

            function handleNextRequest() {
                console.assert(selectedHudEditorModel)
                console.assert(selectedForLoadingFileName)
                if (selectedHudEditorModel.load(selectedForLoadingFileName)) {
                    UI.ui.playForwardSound()
                    stackView.setComponent(hudEditorComponent, {
                        "selectedHudEditorModel" : selectedHudEditorModel,
                        "selectedForLoadingFileName" : selectedForLoadingFileName,
                    })
                } else {
                    console.warn("Failed to load the HUD editor model from", root.impl.selectedForLoadingFileName)
                    root.exitRequested()
                }
            }

            Flickable {
                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.horizontalCenter: parent.horizontalCenter
                width: listItemWidth
                Item {
                    width: listItemWidth
                    height: Math.max(parent.height, selectHudToLoadList.height)
                    ListView {
                        id: selectHudToLoadList
                        width: listItemWidth
                        height: contentHeight
                        anchors.centerIn: parent
                        spacing: 12
                        model: selectedHudEditorModel.existingHuds
                        delegate: SelectableLabel {
                            width: listItemWidth
                            text: modelData
                            selected: loadPage.selectedIndex === index
                            onClicked: {
                                UI.ui.playSwitchSound()
                                selectedForLoadingFileName = modelData
                                loadPage.selectedIndex = index
                            }
                        }
                    }
                }
            }
        }
    }

    Component {
        id: hudEditorComponent
        Item {
            property var selectedHudEditorModel
            property var selectedForLoadingFileName

            readonly property string subpageTitle: "Edit the selected HUD"
            readonly property string summary: {
                const flavorString = (selectedHudEditorModel === UI.regularHudEditorModel) ? "regular" : "miniview"
                '<b>' + selectedForLoadingFileName.toUpperCase() + ' (<font color="yellow">' + flavorString + '</font>)</b>'
            }
            readonly property bool canGoBack: true
            readonly property bool canGoNext: true
            readonly property int stageIndex: 2

            width: root.width

            Component.onCompleted: {
                console.assert(selectedHudEditorModel)
                console.assert(selectedForLoadingFileName)
            }

            function handleBackRequest() {
                console.assert(selectedHudEditorModel)
                console.assert(selectedForLoadingFileName)
                UI.ui.playBackSound()
                stackView.setComponent(loadPageComponent, {"selectedHudEditorModel" : selectedHudEditorModel})
            }

            function handleNextRequest() {
                console.assert(selectedHudEditorModel)
                console.assert(selectedForLoadingFileName)
                UI.ui.playForwardSound()
                stackView.setComponent(savePageComponent, {
                    "selectedHudEditorModel" : selectedHudEditorModel,
                    "selectedForLoadingFileName" : selectedForLoadingFileName,
                })
            }

            HudEditor {
                anchors.centerIn: parent
                width: parent.width
                height: parent.height - 48
                hudEditorModel: selectedHudEditorModel
                Component.onCompleted: {
                    selectedHudEditorModel.setDragAreaSize(width, height)
                    selectedHudEditorModel.setFieldAreaSize(fieldWidth, fieldHeight)
                }
            }
        }
    }

    Component {
        id: savePageComponent
        Item {
            id: savePage

            property var selectedHudEditorModel
            property var selectedForLoadingFileName
            property var selectedForSavingFileName

            readonly property string subpageTitle: 'Select a file to save the edited HUD'
            readonly property string summary: {
                if (selectedForSavingFileName) {
                    if (selectedForSavingFileName === selectedForLoadingFileName) {
                        'You have selected saving changes back to the <b><font color="yellow">' +
                            selectedForLoadingFileName + '</font></b> HUD file'
                    } else if (savePage.selectedIndex == selectedHudEditorModel.existingHuds.length) {
                        'You have selected saving the edited <b><font color="yellow">' + selectedForLoadingFileName +
                        '</font></b> HUD to <b><font color="yellow">' + selectedForSavingFileName + '</font><b>'
                    } else {
                        '<b><font color="red">Caution: </font></b>' +
                        'You have selected overwriting an unrelated existing file by an edited ' +
                        '<b><font color="yellow">' + selectedForLoadingFileName + '</b></font> HUD'
                    }
                } else {
                    ''
                }
            }
            readonly property bool canGoBack: true
            readonly property bool canGoNext: !!selectedForSavingFileName
            readonly property int stageIndex: 3

            property int selectedIndex: -1
            width: root.width

            Component.onCompleted: {
                console.assert(selectedHudEditorModel)
                console.assert(selectedForLoadingFileName)
            }

            function handleBackRequest() {
                console.assert(selectedHudEditorModel)
                console.assert(selectedForLoadingFileName)
                UI.ui.playBackSound()
                stackView.setComponent(hudEditorComponent, {
                    "selectedHudEditorModel" : selectedHudEditorModel,
                    "selectedForLoadingFileName" : selectedForLoadingFileName,
                })
            }

            function handleNextRequest() {
                console.assert(selectedHudEditorModel)
                console.assert(selectedForLoadingFileName)
                console.assert(selectedForSavingFileName)
                if (selectedHudEditorModel.save(selectedForSavingFileName)) {
                    UI.ui.playForwardSound()
                } else {
                    console.warn("Failed to saved the HUD model to", selectedForSavingFileName)
                }
                root.exitRequested()
            }

            onSelectedIndexChanged: {
                if (savePage.selectedIndex >= 0) {
                    // The model could differ from the default/existing one
                    selectedForSavingFileName = selectHudToSaveList.model[savePage.selectedIndex]
                } else {
                    selectedForSavingFileName = undefined
                }
            }

            UICheckBox {
                id: allowOverwritingCheckBox
                anchors.top: parent.top
                anchors.topMargin: 16
                anchors.left: parent.left
                anchors.leftMargin: width / 2
                Material.theme: checked ? Material.Light : Material.Dark
                text: "Allow overwriting existing files"
                onCheckedChanged: {
                    if (!checked && savePage.selectedIndex >= 0) {
                        if (savePage.selectedIndex !== existingHuds.length) {
                            savePage.selectedIndex = -1
                        }
                    }
                }
            }

            Flickable {
                anchors.top: allowOverwritingCheckBox.bottom
                anchors.bottom: parent.bottom
                anchors.horizontalCenter: parent.horizontalCenter
                width: listItemWidth
                Item {
                    width: listItemWidth
                    height: Math.max(parent.height, selectHudToSaveColumn.height)
                    Column {
                        id: selectHudToSaveColumn
                        anchors.centerIn: parent
                        spacing: 8
                        ListView {
                            id: selectHudToSaveList
                            model: selectedHudEditorModel.existingHuds
                            width: listItemWidth
                            height: contentHeight
                            spacing: 12
                            delegate: SelectableLabel {
                                enabled: allowOverwritingCheckBox.checked || index === selectedHudEditorModel.existingHuds.length
                                width: listItemWidth
                                text: modelData
                                selected: savePage.selectedIndex === index
                                onClicked: {
                                    UI.ui.playSwitchSound()
                                    savePage.selectedIndex = index
                                    const existingHuds     = selectedHudEditorModel.existingHuds
                                    // Remove the no longer useful custom name by switching back to the default model
                                    if (model.length != existingHuds.length && index != existingHuds.length) {
                                        selectHudToSaveList.model = existingHuds
                                    }
                                }
                            }
                        }
                        AddNewListItemButton {
                            width: listItemWidth
                            height: 36
                            maxHudNameLength: selectedHudEditorModel.maxHudNameLength
                            onAdditionRequested: {
                                UI.ui.playForwardSound()
                                const model = [...selectedHudEditorModel.existingHuds]
                                model.push(text)
                                selectHudToSaveList.model = model
                                savePage.selectedIndex = model.length - 1
                                // Must do that manually since the index could remain the same
                                selectedForSavingFileName = text
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
        count: 4
        currentIndex: stackView.currentItem.stageIndex
        interactive: false
    }

    RowLayout {
        id: buttonsBar
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 32
        width: UI.acceptRejectRowWidthFrac * parent.width
        height: 64

        SlantedLeftSecondaryButton {
            id: backButton
            text: "back"
            visible: stackView.currentItem.canGoBack
            onClicked: stackView.currentItem.handleBackRequest()
        }

        Item {
            Layout.preferredWidth: backButton.Layout.preferredWidth
            visible: !backButton.visible
        }

        Item { Layout.fillWidth: true }

        SlantedRightPrimaryButton {
            id: nextButton
            highlighted: true
            text: stackView.currComponent === savePageComponent ? "save" : "next"
            visible: stackView.currentItem.canGoNext
            onClicked: stackView.currentItem.handleNextRequest()
        }

        Item {
            Layout.preferredWidth: backButton.Layout.preferredWidth
            visible: !nextButton.visible
        }
    }

    function handleKeyEvent(event) {
        if (event.key === Qt.Key_Escape || event.key === Qt.Key_Back) {
            if (stackView.currentItem.canGoBack) {
                stackView.currentItem.handleBackRequest()
            } else {
                root.exitRequested()
            }
            event.accepted = true
            return true
        }
        return false
    }
}