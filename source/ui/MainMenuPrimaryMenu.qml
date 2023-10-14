import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import QtGraphicalEffects 1.12
import net.warsow 2.6

Item {
	id: root

    property bool shouldShowExpandedButtons

    readonly property bool areButtonsVisible: shouldShowExpandedButtons || someOverlayButton.expansionFrac < 1.0

	property real expansionFrac: someOverlayButton.expansionFrac

    property int activePageTag: 0
    property int selectedPageTag: 0

    readonly property int highlightedPageTag: activePageTag ? activePageTag : selectedPageTag

    readonly property int pageTagMin: 1
    readonly property int pageNews: 1
    readonly property int pageProfile: 2
    readonly property int pagePlayOnline: 3
    readonly property int pageLocalGame: 4
    readonly property int pageSettings: 5
    readonly property int pageDemos: 6
    readonly property int pageHelp: 7
    readonly property int pageQuit: 8
    readonly property int pageTagMax: 8

	Item {
		id: logoHolder
		anchors.centerIn: parent
		width: logo.width + 32
		height: logo.height + 32
		//opacity: 1.0 - logoUnderlay.animFrac

        Component {
            id: glowComponent
            Item {
            Rectangle {
                id: logoUnderlay
                visible: false
                anchors.centerIn: parent
                width: root.width / 2
                height: root.height / 2
                opacity: 0.3
            }
            Glow {
                anchors.fill: logoUnderlay
                fast: true
                enabled: true
                radius: 3
                samples: 7
                color: "magenta"
                source: logoUnderlay
            }
            }
        }

        Loader {
            anchors.centerIn: parent
            width: logo.width
            height: logo.height
            active: true
            sourceComponent: glowComponent
            /*
            Timer {
                running: true
                interval: 10000
                onTriggered: parent.active = true
            }*/
        }

        /*
        DirectionalBlur {
            anchors.fill: logoUnderlay
            source: logoUnderlay
            //radius: 16
            angle: 90
            length: 0.5 * logoUnderlay.extraLength
            samples: 12 + 100 * logoUnderlay.animFrac
        }*/

		Logo {
		    id: logo
		    anchors.centerIn: parent
		    width: root.width
		    height: logo.implicitHeight
		}
	}

	ColumnLayout {
		id: topColumn
		visible: areButtonsVisible
		anchors.top: logoHolder.bottom
		anchors.left: parent.left
		anchors.right: parent.right
		spacing: 18

		MainMenuButtonRow {
		    highlighted: root.highlightedPageTag === root.pageNews
			id: someOverlayButton
			text: "News"
			leaningRight: true
			Layout.fillWidth: true
			enableSlidingOvershoot: root.shouldShowExpandedButtons
			onClicked: root.handleButtonClicked(root.pageNews)
			onExpansionFracChanged: logoHolder.opacity = 1.0 - someOverlayButton.expansionFrac
		}

		MainMenuButtonRow {
		    highlighted: root.highlightedPageTag === root.pageProfile
			text: "Profile"
			leaningRight: true
			Layout.fillWidth: true
			enableSlidingOvershoot: root.shouldShowExpandedButtons
			onClicked: root.handleButtonClicked(root.pageProfile)
		}

		MainMenuButtonRow {
		    highlighted: root.highlightedPageTag === root.pagePlayOnline
			text: "Play online"
			leaningRight: true
			Layout.fillWidth: true
			enableSlidingOvershoot: root.shouldShowExpandedButtons
			onClicked: root.handleButtonClicked(root.pagePlayOnline)
		}

		MainMenuButtonRow {
		    highlighted: root.highlightedPageTag === root.pageLocalGame
		    highlightedWithAnim: UI.ui.isClientDisconnected
			text: "Local game"
			leaningRight: true
			Layout.fillWidth: true
			enableSlidingOvershoot: root.shouldShowExpandedButtons
			onClicked: root.handleButtonClicked(root.pageLocalGame)
		}
	}

	ColumnLayout {
		id: bottomColumn
		visible: areButtonsVisible
		anchors.bottom: logoHolder.top
		anchors.left: parent.left
		anchors.right: parent.right
		spacing: 16

		MainMenuButtonRow {
		    highlighted: root.highlightedPageTag === root.pageSettings
			text: "Settings"
			leaningRight: false
			Layout.fillWidth: true
			enableSlidingOvershoot: root.shouldShowExpandedButtons
			onClicked: root.handleButtonClicked(root.pageSettings)
		}

		MainMenuButtonRow {
		    highlighted: root.highlightedPageTag === root.pageDemos
			text: "Demos"
			leaningRight: false
			Layout.fillWidth: true
			enableSlidingOvershoot: root.shouldShowExpandedButtons
			onClicked: root.handleButtonClicked(root.pageDemos)
		}

		MainMenuButtonRow {
		    highlighted: root.highlightedPageTag === root.pageHelp
			text: "Help"
			leaningRight: false
			Layout.fillWidth: true
			enableSlidingOvershoot: root.shouldShowExpandedButtons
			onClicked: root.handleButtonClicked(root.pageHelp)
		}

		MainMenuButtonRow {
		    highlighted: root.highlightedPageTag === root.pageQuit
			text: "Quit"
			leaningRight: false
			Layout.fillWidth: true
			enableSlidingOvershoot: root.shouldShowExpandedButtons
			onClicked: root.handleButtonClicked(root.pageQuit)
		}
	}

	function toggleExpandedState() {
		for (let i = 0; i < topColumn.children.length; ++i) {
			topColumn.children[i].toggleExpandedState()
		}
		for (let i = 0; i < bottomColumn.children.length; ++i) {
			bottomColumn.children[i].toggleExpandedState()
		}
	}

    function handleButtonClicked(pageTag) {
        if (!activePageTag) {
            toggleExpandedState()
            activePageTag = pageTag
            selectedPageTag = 0
        } else if (pageTag === activePageTag) {
            toggleExpandedState()
            activePageTag = 0
            selectedPageTag = pageTag
        } else {
            activePageTag = pageTag
        }
    }

    function handleKeyBack() {
        if (selectedPageTag) {
            selectedPageTag = 0
            return true
        }
        if (activePageTag) {
            toggleExpandedState()
            selectedPageTag = activePageTag
            activePageTag = 0
            return true
        }
        return false
    }

    function handleKeyEnter() {
        if (!selectedPageTag) {
            return false
        }
        toggleExpandedState()
        activePageTag = selectedPageTag
        selectedPageTag = 0
        return true
    }

    function handleKeyTab() {
        if (activePageTag) {
            return false
        }
        if (selectedPageTag) {
            selectedPageTag = 0
        } else {
            selectedPageTag = pageSettings
        }
        return true
    }

    function wrapPageTag(tag) {
        if (tag < pageTagMin) {
            return pageTagMax
        }
        if (tag > pageTagMax) {
            return pageTagMin
        }
        return tag
    }

    function handleKeyUpOrDown(delta) {
        if (activePageTag) {
            activePageTag = wrapPageTag(activePageTag + delta)
            return true
        }
        if (selectedPageTag) {
            selectedPageTag = wrapPageTag(selectedPageTag + delta)
            return true
        }
        return false
    }

    function _handleKeyEvent(event) {
        if (!shouldShowExpandedButtons && expansionFrac > 0.0) {
            if (event.key === Qt.Key_Escape) {
                return handleKeyBack()
            }
            return false
        }
        switch (event.key) {
            case Qt.Key_Escape: return handleKeyBack()
            case Qt.Key_Tab: return handleKeyTab()
            case Qt.Key_Enter: return handleKeyEnter()
            case Qt.Key_Up: return handleKeyUpOrDown(-1)
            case Qt.Key_Down: return handleKeyUpOrDown(+1)
            default: return false
        }
    }

    function handleKeyEvent(event) {
        if (_handleKeyEvent(event)) {
            event.accepted = true
            return true
        }
        return false
    }
}
