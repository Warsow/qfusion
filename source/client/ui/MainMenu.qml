import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtGraphicalEffects 1.12
import net.warsow 2.6

Item {
	id: root

	property alias expansionFrac: decoratedLogo.expansionFrac

	readonly property bool tabVisible: expansionFrac < 1.0
	readonly property bool tabEnabled: expansionFrac <= 1.0
	readonly property real tabOpacity: 1.0 - expansionFrac

    TabBar {
        id: menuTabBar
        visible: tabVisible
        enabled: tabEnabled
        opacity: tabOpacity
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter
        width: 0.6 * contentPane.width
        height: UI.tabHeight
        background: null
        Component.onCompleted: {
            root.resetTabBarState(menuTabBar)
            menuTabBar.contentItem.highlight = null
        }
        UITabButton {
            text: "Play online"
            width: 0.25 * menuTabBar.width
            onClicked: root.selectComponent(playOnlineComponent)
        }
        UITabButton {
            text: "Local game"
            width: 0.25 * menuTabBar.width
            onClicked: root.selectComponent(localGameComponent)
        }
        UITabButton {
            text: "Demos"
            width: 0.25 * menuTabBar.width
            onClicked: root.selectComponent(demosComponent)
        }
        UITabButton {
            text: "Settings"
            width: 0.25 * menuTabBar.width
            onClicked: root.selectComponent(settingsComponent)
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
            text: "Quit"
            width: 0.1 * contentPane.width
            onClicked: root.selectComponent(quitComponent)
        }
    }

    function selectComponent(c) {
        decoratedLogo.toggleExpandedState()
        // replace the entire stack
        contentPane.replace(null, c)
    }

	Component {
	    id: playOnlineComponent
	    PlayOnlinePage {
	        onPlayingLocallySuggested: {
	            UI.ui.playForwardSound()
	            contentPane.replace(null, localGameComponent)
	        }
	    }
	}

	Component {
	    id: localGameComponent
	    LocalGamePage {}
	}

    Component {
        id: settingsComponent
        SettingsPage {}
    }

    Component {
        id: demosComponent
        DemosPage {}
    }

    Component {
        id: quitComponent
        QuitPage {
            backTrigger: () => collapse()
        }
    }

    StackView {
		id: contentPane
		hoverEnabled: expansionFrac >= 1.0
		opacity: expansionFrac
		anchors.top: parent.top
		anchors.bottom: parent.bottom
		anchors.horizontalCenter: parent.horizontalCenter
		width: 1024 + 128
	}

	MainMenuDecoratedLogo {
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

    function collapse() {
        decoratedLogo.toggleExpandedState()
        contentPane.clear()
        resetTabBarState(menuTabBar)
        resetTabBarState(quitTabBar)
        root.forceActiveFocus()
        UI.ui.playBackSound()
    }

	Keys.onPressed: {
	    const currentPaneItem = contentPane.currentItem
	    // TODO: Events propagation needs some attention and some work, e.g. setting the .accepted flag
	    // TODO: Check whether Keys.redirectTo is applicable
	    if (currentPaneItem) {
	        if (currentPaneItem.hasOwnProperty("handleKeyEvent")) {
	            const handler = currentPaneItem.handleKeyEvent
	            if (handler && handler(event)) {
	                console.assert(event.accepted)
	                return
	            }
	        }
	    }
	    if (event.key === Qt.Key_Escape) {
	        if (contentPane.empty) {
	            UI.ui.returnFromMainMenu()
	        } else {
	            collapse()
	        }
	        event.accepted = true
	    }
	}
}