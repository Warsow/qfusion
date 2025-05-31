import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtGraphicalEffects 1.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

// TODO: Unify with SlantedGametypeOption
Item {
    id: root

    signal clicked()

    implicitWidth: label.implicitWidth + iconOrPlaceholder.implicitWidth + 20 + 16
    implicitHeight: UI.regularSlantedButtonHeight
    height: implicitHeight

    property string text
    property string iconPath

    property bool highlighted
    property bool highlightedWithAnim
    property bool checked
    property bool displayIconPlaceholder: false
    property bool highlightOnActiveFocus: true
    property alias font: label.font

    property real iconWidthAndHeight: 20
    property real iconOrPlaceholderLeftMargin: 20
    property real labelLeftMargin: 12
    property real labelHorizontalCenterOffset: -12

    property real placeholderSlantDegrees: UI.buttonBodySlantDegrees - 1.0
    property real leftBodyPartSlantDegrees: UI.buttonBodySlantDegrees
    property real rightBodyPartSlantDegrees: UI.buttonBodySlantDegrees
    property real textSlantDegrees: UI.buttonTextSlantDegrees

    property real cornerRadius: 3

    property real extraWidthOnMouseOver: 12.0
    property real extraHeightOnMouseOver: 2.0

    property real highlightAnimAmplitude: 5.0
    property int highlightInterval: 5000

    readonly property bool hasActiveHighlight:
        mouseArea.containsMouse || root.highlighted || root.checked || highlightAnim.highlightActive || (root.highlightOnActiveFocus && root.activeFocus)

    readonly property var translationMatrix:
        UI.ui.makeTranslateMatrix(highlightAnim.running ? highlightAnimAmplitude * highlightAnim.bodyShiftFrac : 0.0, 0.0)

    states: [
        State {
            name: "leftAligned"
            when: iconPath.length > 0 || displayIconPlaceholder
            AnchorChanges {
                target: label
                anchors.left: iconOrPlaceholder.right
                anchors.horizontalCenter: undefined
                anchors.verticalCenter: body.verticalCenter
            }
            PropertyChanges {
                target: label
                anchors.leftMargin: labelLeftMargin
                anchors.horizontalCenterOffset: 0
            }
        },
        State {
            name: "centerAligned"
            when: iconPath.length === 0 && !displayIconPlaceholder
            AnchorChanges {
                target: label
                anchors.left: undefined
                anchors.horizontalCenter: body.horizontalCenter
                anchors.verticalCenter: body.verticalCenter
            }
            PropertyChanges {
                target: label
                anchors.leftMargin: 0
                anchors.horizontalCenterOffset: labelHorizontalCenterOffset
            }
        }
    ]

    ButtonHighlightAnim {
        id: highlightAnim
        highlightInterval: root.highlightInterval
        running: root.highlightedWithAnim && !mouseArea.containsMouse && !mouseLeftTimer.running && !UI.ui.isConsoleOpen
    }

    Timer {
        id: mouseLeftTimer
        interval: 10000
    }

    MouseArea {
        id: mouseArea
        hoverEnabled: true
        anchors.fill: parent
        onClicked: root.clicked()
        onContainsMouseChanged: {
            if (containsMouse) {
                UI.ui.playHoverSound()
            } else {
                mouseLeftTimer.start()
            }
        }
    }

    Keys.onEnterPressed: root.clicked()

    SlantedBackground {
        id: body
        anchors.centerIn: parent
        width: (mouseArea.containsMouse || highlightAnim.highlightActive) && root.extraWidthOnMouseOver ? parent.width + root.extraWidthOnMouseOver : parent.width
        height: (mouseArea.containsMouse || highlightAnim.highlightActive) && root.extraHeightOnMouseOver ? parent.height + root.extraHeightOnMouseOver : parent.height
        radius: root.cornerRadius
        leftPartSkewDegrees: root.leftBodyPartSlantDegrees
        rightPartSkewDegrees: root.rightBodyPartSlantDegrees
        shadowOpacity: Math.min(1.0, width / parent.width)
        enabled: root.enabled

        transform: Matrix4x4 { matrix: translationMatrix }

        fillColor: !root.enabled ? "darkgrey" : (hasActiveHighlight ? Material.accentColor : Qt.lighter(Material.backgroundColor, 1.35))
        opacity: !root.enabled ? 0.2 : 1.0

        Behavior on width { SmoothedAnimation { duration: 333 } }
        Behavior on height { SmoothedAnimation { duration: 333 } }
        Behavior on fillColor { ColorAnimation { duration: highlightAnim.colorAnimDuration } }
    }

    Component {
        id: placeholderComponent
        Rectangle {
            width: 12
            height: 12
            implicitWidth: 12
            implicitHeight: 12
            radius: 1
            opacity: root.enabled ? (mouseArea.containsMouse ? 1.0 : 0.7) : 0.33
            transform: Matrix4x4 { matrix: UI.ui.makeSkewXMatrix(height, placeholderSlantDegrees).times(translationMatrix) }
        }
    }

    Component {
        id: iconComponent
        Item {
            width: root.iconWidthAndHeight
            height: root.iconWidthAndHeight
            implicitWidth: root.iconWidthAndHeight
            implicitHeight: root.iconWidthAndHeight

            Image {
                id: icon
                visible: !root.hasActiveHighlight
                anchors.centerIn: parent
                width: root.iconWidthAndHeight
                height: root.iconWidthAndHeight
                smooth: true
                mipmap: true
                source: root.iconPath
            }

            ColorOverlay {
                visible: root.hasActiveHighlight
                anchors.fill: icon
                source: icon
                color: "white"
            }
        }
    }

    Loader {
        id: iconOrPlaceholder
        active: root.displayIconPlaceholder
        anchors.left: body.left
        anchors.verticalCenter: parent.verticalCenter
        anchors.leftMargin: root.iconOrPlaceholderLeftMargin
        width: iconOrPlaceholder.item ? iconOrPlaceholder.item.implicitWidth : 0
        height: iconOrPlaceholder.item ? iconOrPlaceholder.item.implicitHeight : 0
        sourceComponent: iconPath.length > 0 ? iconComponent : placeholderComponent
    }

    UILabel {
        id: label

        text: root.text
        font.weight: Font.ExtraBold
        font.letterSpacing: 1.5
        font.capitalization: Font.AllUppercase

        transform: Matrix4x4 { matrix: UI.ui.makeSkewXMatrix(label.height, textSlantDegrees).times(translationMatrix) }
        opacity: root.enabled ? 1.0 : 0.5
    }
}