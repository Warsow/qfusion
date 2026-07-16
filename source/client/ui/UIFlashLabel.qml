import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Label {
    id: root

    function enter() {
        console.assert(!_hovered)
        _hovered = true
        flash()
    }
    function leave() {
        console.assert(_hovered)
        _hovered = false
    }
    function flash() {
        flashAnim.start()
    }

    SequentialAnimation {
        id: flashAnim
        ParallelAnimation {
            NumberAnimation {
                target: flashScale
                property: "xScale"
                to: 1.0 + (flashAlignment == Qt.AlignHCenter ? 2.0 : 1.0) *
                    (root.implicitWidth ? Math.min(root.maxFlashWidthOvershootFrac, root.maxFlashWidthOvershoot / root.implicitWidth) : 0.0)
                duration: 50
            }
            NumberAnimation {
                target: flashScale
                property: "yScale"
                // See the remark below
                to: 1.0 + 2.0 * (root.implicitHeight ? Math.min(root.maxFlashHeightOvershootFrac, root.maxFlashHeightOvershoot / root.implicitHeight) : 0.0)
                duration: 50
            }
        }
        NumberAnimation {
            target: flashScale
            properties: "xScale,yScale"
            to: 1.0
            duration: 125
        }
    }

    property real extraFontSizeOnHover: 0.0
    property real extraFontSpacingOnHover: 1.5

    property real maxFlashWidthOvershoot: 10.0
    property real maxFlashWidthOvershootFrac: 1.2

    // Note: Y scale is always applied from the center. It is auxiliary.
    // This control is primarily about displaying a horizontal flash.
    property real maxFlashHeightOvershoot: 4.0
    property real maxFlashHeightOvershootFrac: 1.1

    property real flashAlignment: Qt.AlignHCenter

    // TODO: Put into QtObject { id: impl ... } ?
    // TODO: Move to flashLabel
    property bool _hovered: false

    font.weight: UI.labelFontWeight
    font.pointSize: UI.labelFontSize + (_hovered ? extraFontSizeOnHover : 0.0)
    font.letterSpacing: UI.labelLetterSpacing + (_hovered ? extraFontSpacingOnHover : 0.0)

    Behavior on font.pointSize { SmoothedAnimation { duration: 135 } }
    // TODO: Just stretch instead of using spacing?
    Behavior on font.letterSpacing { SmoothedAnimation { duration: 135 } }

    Behavior on color { ColorAnimation { duration: 50 } }

    Label {
        id: flashLabel
        z: -1
        anchors.fill: parent
        verticalAlignment: root.verticalAlignment
        horizontalAlignment: root.horizontalAlignment

        font: root.font
        color: root.color
        opacity: 0.4
        visible: root.enabled && flashAnim.running

        // TODO: Take padding into account
        readonly property real clippedContentWidth: contentWidth > width ? width : contentWidth
        readonly property real clippedContentHeight: contentHeight > height ? height : contentHeight

        readonly property real contentX: (horizontalAlignment === Qt.AlignHCenter) ? 0.5 * (width - clippedContentWidth) :
            ((horizontalAlignment === Qt.AlignLeft) ? 0.0 : width - clippedContentWidth)
        readonly property real contentY: (verticalAlignment === Qt.AlignVCenter) ? 0.5 * (height - clippedContentHeight) :
            ((verticalAlignment === Qt.AlignTop) ? 0.0 : height - clippedContentHeight)

        text: parent.text

        transform: [
            Scale {
                id: flashScale
                origin.y: flashLabel.contentY + 0.5 * flashLabel.clippedContentHeight
            }
        ]

        states: [
            State {
                name: "centerAligned"
                when: root.flashAlignment === Qt.AlignHCenter
                PropertyChanges {
                    target: flashScale
                    origin.x: flashLabel.contentX + 0.5 * flashLabel.clippedContentWidth
                }
            },
            State {
                name: "rightAligned"
                when: root.flashAlignment === Qt.AlignRight
                PropertyChanges {
                    target: flashScale
                    origin.x: flashLabel.contentX + flashLabel.clippedContentWidth
                }
            },
            State {
                name: "leftAligned"
                when: root.flashAlignment === Qt.AlignLeft
                PropertyChanges {
                    target: flashScale
                    origin.x: flashLabel.contentX
                }
            }
        ]
    }
}