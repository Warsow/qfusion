import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
	id: root

	property real expansionFrac: someOverlayButton.expansionFrac
	readonly property real spacing: 12

	Item {
		id: logoHolder
		anchors.centerIn: parent
		width: logo.width
		height: logo.height
		Image {
			id: logo
			anchors.centerIn: parent
			source: "image://wsw/gfx/ui/loadinglogo"
		}
	}

	ColumnLayout {
		id: topColumn
		anchors.top: logoHolder.bottom
		anchors.left: parent.left
		anchors.right: parent.right
		spacing: UI.logoDecorationRowSpacing

		MainMenuButtonRow {
			id: someOverlayButton
			leaningRight: true
			Layout.fillWidth: true
			onExpansionFracChanged: logoHolder.opacity = 1.0 - someOverlayButton.expansionFrac
		}

		MainMenuButtonRow {
			leaningRight: true
			Layout.fillWidth: true
		}

		MainMenuButtonRow {
			leaningRight: true
			Layout.fillWidth: true
		}

		MainMenuButtonRow {
			leaningRight: true
			Layout.fillWidth: true
		}
	}

	ColumnLayout {
		id: bottomColumn
		anchors.bottom: logoHolder.top
		anchors.left: parent.left
		anchors.right: parent.right
		spacing: UI.logoDecorationRowSpacing

		MainMenuButtonRow {
			leaningRight: false
			Layout.fillWidth: true
		}

		MainMenuButtonRow {
			leaningRight: false
			Layout.fillWidth: true
		}

		MainMenuButtonRow {
			leaningRight: false
			Layout.fillWidth: true
		}

		MainMenuButtonRow {
			leaningRight: false
			Layout.fillWidth: true
		}
	}

	function toggleExpandedState() {
	    // TODO: Should we perform this here?
	    if (someOverlayButton.state === "centered") {
	        UI.ui.playForwardSound()
	    } else {
	        UI.ui.playBackSound()
	    }

		for (let i = 0; i < topColumn.children.length; ++i) {
			topColumn.children[i].toggleExpandedState()
		}
		for (let i = 0; i < bottomColumn.children.length; ++i) {
			bottomColumn.children[i].toggleExpandedState()
		}
	}
}
