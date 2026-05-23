import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
	id: root
	height: UI.logoDecorationRowHeight

    property bool highlighted: false
	property string text
	property bool leaningRight: false
	property real expansionFrac: 0.0

	function toggleExpandedState() {
		if (state == "centered") {
			state = leaningRight ? "goneLeft" : "goneRight"
		} else {
			state = "centered"
		}
	}

	readonly property var transformMatrix: UI.ui.makeSkewXMatrix(root.height, 20.0)

	readonly property color foregroundColor: "white"
	readonly property color trailDecayColor: UI.ui.colorWithAlpha(Material.backgroundColor, 0.0)
	readonly property color highlightedColor: Material.accentColor

    // These properties used to depend of mouse/anim timers

	readonly property real bodyWidth: UI.mainMenuButtonWidth
	readonly property real bodyHeight: root.height
	readonly property color bodyColor: foregroundColor

    // Seems to be useful to keep in the source, even commented out

	//Behavior on bodyWidth { SmoothedAnimation { duration: 333 } }
	//Behavior on bodyHeight { SmoothedAnimation { duration: 333 } }
	//Behavior on bodyColor { ColorAnimation { duration: highlightAnim.colorAnimDuration } }

    readonly property real baseTrailElementWidth: 24
    readonly property real trailSpacing: 4
    readonly property real trailElementWidth: baseTrailElementWidth
	readonly property int trailElementsCount: Math.floor(UI.mainMenuButtonTrailWidth / (baseTrailElementWidth + root.trailSpacing))

    //Behavior on trailElementWidth {
    //    NumberAnimation {
    //        duration: 1000
    //        easing.type: Easing.OutElastic
    //        easing.amplitude: 3.0
    //    }
    //}

	states: [
		State {
			name: "centered"
			AnchorChanges {
				target: body
				anchors.horizontalCenter: root.horizontalCenter
				anchors.verticalCenter: root.verticalCenter
				anchors.left: undefined
				anchors.right: undefined
			}
			PropertyChanges {
			    target: body
			    anchors.leftMargin: 0
			    anchors.rightMargin: 0
			}
		},
		State {
			name: "goneLeft"
			AnchorChanges {
				target: body
				anchors.horizontalCenter: undefined
				anchors.verticalCenter: root.verticalCenter
				anchors.left: undefined
				anchors.right: root.left
		    }
		    PropertyChanges {
		        target: body
		        anchors.leftMargin: 0
				anchors.rightMargin: UI.mainMenuButtonTrailWidth
			}
		},
		State {
			name: "goneRight"
			AnchorChanges {
				target: body
				anchors.horizontalCenter: undefined
				anchors.verticalCenter: root.verticalCenter
				anchors.left: root.right
				anchors.right: undefined
			}
			PropertyChanges {
			    target: body
				anchors.leftMargin: UI.mainMenuButtonTrailWidth
			    anchors.rightMargin: 0
			}
		}
	]

	transitions: [
	    Transition {
		    AnchorAnimation {
			    duration: 333
			    easing.type: Easing.OutBack
		    }
	    }
	]

	state: "centered"

	Loader {
		active: !leaningRight
		anchors.right: body.left
		anchors.rightMargin: trailSpacing
	    anchors.verticalCenter: body.verticalCenter
		sourceComponent: leftTrailComponent
	}

	Component {
		id: leftTrailComponent
		MainMenuButtonTrail {
            spacing: root.trailSpacing
            elementsCount: root.trailElementsCount
            elementWidth: trailElementWidth
            elementHeight: root.bodyHeight
			leftColor: root.bodyColor
			rightColor: root.trailDecayColor
			transformMatrix: root.transformMatrix
		}
	}

    // The trail does not have a shadow but this is sufficent
    Item {
        z: -1
        id: shadowCaster
        anchors.centerIn: body
        opacity: 0.5
        Item {
            anchors.centerIn: parent
            width: body.width
            height: body.height
		    transform: Matrix4x4 { matrix: root.transformMatrix }
            layer.enabled: true
            layer.effect: ElevationEffect { elevation: 16 }
        }
    }

	Rectangle {
		id: body
		width: bodyWidth
		height: bodyHeight
		radius: 3
		color: bodyColor

		transform: Matrix4x4 { matrix: root.transformMatrix }

		onXChanged: {
			const halfContainerWidth = parent.width / 2
			const halfThisWidth = width / 2
			const slidingDistance = halfContainerWidth - halfThisWidth
			let frac = 0.0
			if (root.leaningRight) {
				frac = Math.abs(x - halfContainerWidth + halfThisWidth) / slidingDistance
			} else {
				frac = Math.abs(x - halfContainerWidth + halfThisWidth) / slidingDistance
			}
			root.expansionFrac = Math.min(1.0, Math.max(frac, 0.0))
		}

		UILabel {
			anchors.left: root.leaningRight ? parent.left: undefined
			anchors.right: root.leaningRight ? undefined: parent.right
			anchors.verticalCenter: parent.verticalCenter
			anchors.leftMargin: 12
			anchors.rightMargin: 12
			font.family: UI.ui.headingFontFamily
			font.letterSpacing: 1.5
			text: root.text
			font.weight: Font.ExtraBold
			font.capitalization: Font.AllUppercase
		}
	}

	Loader {
		active: leaningRight
		anchors.left: body.right
		anchors.leftMargin: trailSpacing
		anchors.verticalCenter: body.verticalCenter
		sourceComponent: rightTrailComponent
	}

	Component {
		id: rightTrailComponent
		MainMenuButtonTrail {
			spacing: root.trailSpacing
			elementsCount: root.trailElementsCount
			elementWidth: trailElementWidth
			elementHeight: bodyHeight
			leftColor: root.trailDecayColor
			rightColor: root.bodyColor
			transformMatrix: root.transformMatrix
		}
	}
}
