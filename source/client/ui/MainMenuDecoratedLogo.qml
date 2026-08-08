import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
	id: root

	readonly property real expansionFrac: logoHolder.expansionFrac
	readonly property real spacing: 12

    // TODO: Should we really start it expanded?
	Component.onCompleted: collapse()

	Item {
		id: logoHolder
		anchors.centerIn: parent
		width: root.width
		height: logo.implicitHeight
		// TODO: Using logoHolder as a private part holder
		property real expansionFrac
		property real animatedOpacity
		// The builtin easing is insufficient. Consider using custom animations instead of transitions?
		readonly property real logoOpacity: animatedOpacity * animatedOpacity * animatedOpacity
		readonly property real rowOpacity: animatedOpacity * animatedOpacity
		Image {
			id: logo
			anchors.centerIn: parent
			source: "image://wsw/gfx/ui/loadinglogo"
			width: implicitWidth
			height: implicitHeight
			opacity: logoHolder.logoOpacity
			transform: Scale {
			    id: logoScale
			    origin.x: 0.5 * logo.width
			    origin.y: 0.5 * logo.height
			}
		}
		// Note: We cannot specify implicitWidth, implicitHeight in states
		// as they aren't known at the moment of start of an initial transition.
		// We have to use just some huge values for scale
		states: [
		    State {
		        name: "regular"
		        PropertyChanges {
		            target: logoScale
		            xScale: 1.0
		            yScale: 1.0
		        }
		        PropertyChanges {
		            target: logoHolder
		            animatedOpacity: 1.0
		            expansionFrac: 0.0
		        }
		    },
		    State {
		        name: "expanded"
		        PropertyChanges {
		            target: logoScale
		            xScale: 5.0
		            yScale: 0.0
		        }
		        PropertyChanges {
		            target: logoHolder
		            animatedOpacity: 0.0
		            expansionFrac: 1.0
		        }
		    }
		]
		transitions: Transition {
            NumberAnimation {
                target: logoScale
                properties: "xScale,yScale"
                duration: UI.logoTransitionDuration
                easing.type: Easing.OutQuart
            }
            NumberAnimation {
                target: logoHolder
                property: "animatedOpacity"
                duration: UI.logoTransitionDuration
                easing.type: Easing.OutQuart
            }
            NumberAnimation {
                target: logoHolder
                property: "expansionFrac"
                duration: UI.logoTransitionDuration
            }
        }
        state: "expanded"
	}

	ColumnLayout {
		id: topColumn
		anchors.top: logoHolder.bottom
		anchors.left: parent.left
		anchors.right: parent.right
		spacing: UI.logoDecorationRowSpacing

        Repeater {
            model: 4
		    delegate: MainMenuButtonRow {
			    leaningRight: true
			    Layout.fillWidth: true
			    state: "goneLeft"
			    opacity: logoHolder.rowOpacity
		    }
		}
	}

	ColumnLayout {
		id: bottomColumn
		anchors.bottom: logoHolder.top
		anchors.left: parent.left
		anchors.right: parent.right
		spacing: UI.logoDecorationRowSpacing

        Repeater {
            model: 4
		    delegate: MainMenuButtonRow {
			    leaningRight: false
			    Layout.fillWidth: true
			    state: "goneRight"
			    opacity: logoHolder.rowOpacity
			}
		}
	}

    function _doForEveryRow(fn) {
        // TODO: Cache/flatten acceptable children references?
		for (let i = 0; i < topColumn.children.length; ++i) {
		    if (topColumn.children[i] instanceof MainMenuButtonRow) {
			    fn(topColumn.children[i])
			}
		}
		for (let i = 0; i < bottomColumn.children.length; ++i) {
		    if (bottomColumn.children[i] instanceof MainMenuButtonRow) {
			    fn(bottomColumn.children[i])
			}
		}
    }

    function expand() {
        _doForEveryRow(r => r.expand())
	    logoHolder.state = "expanded"
    }

	function collapse() {
	    _doForEveryRow(r => r.collapse())
        logoHolder.state = "regular"
	}
}
