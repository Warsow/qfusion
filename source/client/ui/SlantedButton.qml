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
    property real labelLeftMargin: 12
    property real labelHorizontalCenterOffset: -12

    // Defaults for general in-game menu buttons (cba to create a specialized component for that)
    property real leftBodyPartSlantDegrees: -0.7 * UI.maxButtonBodySlantDegrees
    property real rightBodyPartSlantDegrees: +0.7 * UI.maxButtonBodySlantDegrees
    // Looks better if it's slightly slanted
    property real textSlantDegrees: UI.neutralButtonTextSlantDegrees

    property real cornerRadius: 4

    property real extraWidthOnMouseOver: 12.0
    property real extraHeightOnMouseOver: 2.0

    property real extraWidthOnFlash: 12.0
    property real extraHeightOnFlash: 2.0

    property real extraLabelWidthOnFlash: 20.0
    property real extraLabelHeightOnFlash: 8.0

    property real iconFlashExtraWidthAndHeight: 12.0
    // Cba to figure out a better name
    property real _iconFlashExtraWidthAndHeight: 0.0

    property real highlightAnimAmplitude: 5.0
    property int highlightInterval: 5000

    readonly property bool hasActiveHighlight:
        mouseArea.containsMouse || root.highlighted || root.checked || highlightAnim.highlightActive || (root.highlightOnActiveFocus && root.activeFocus)

    readonly property var translationMatrix:
        UI.ui.makeTranslateMatrix(highlightAnim.running ? highlightAnimAmplitude * highlightAnim.bodyShiftFrac : 0.0, 0.0)

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
                label.enter()
                leaveAnim.stop()
                // TODO: Should we reassing actual properties for flash anim?
                // Keeping it as is is sufficient as it's very quick but this approach is not really correct.
                flashAnim.start()
            } else {
                mouseLeftTimer.start()
                label.leave()
                if (!flashAnim.running) {
                    startLeaveAnim()
                }
            }
        }
    }

    Keys.onEnterPressed: root.clicked()

    SequentialAnimation {
        id: flashAnim
        ParallelAnimation {
            NumberAnimation {
                target: body
                property: "width"
                from: root.width
                to: root.width + root.extraWidthOnMouseOver + root.extraWidthOnFlash
                duration: 50
            }
            NumberAnimation {
                target: body
                property: "height"
                from: root.height
                to: root.height + root.extraHeightOnMouseOver + root.extraHeightOnFlash
                duration: 50
            }
            NumberAnimation {
                target: root
                property: "_iconFlashExtraWidthAndHeight"
                from: 0.0
                to: root.iconFlashExtraWidthAndHeight
                duration: 50
            }
        }
        ParallelAnimation {
            NumberAnimation {
                target: body
                property: "width"
                from: root.width + root.extraWidthOnMouseOver + root.extraWidthOnFlash
                to: root.width + root.extraWidthOnMouseOver
                duration: 125
            }
            NumberAnimation {
                target: body
                property: "height"
                from: root.height + root.extraHeightOnMouseOver + root.extraHeightOnFlash
                to: root.height + root.extraHeightOnMouseOver
                duration: 125
            }
            NumberAnimation {
                target: root
                property: "_iconFlashExtraWidthAndHeight"
                from: root.iconFlashExtraWidthAndHeight
                to: 0.0
                duration: 125
            }
        }
        onRunningChanged: {
            if (!running && !mouseArea.containsMouse) {
                startLeaveAnim()
            }
        }
    }
    ParallelAnimation {
        id: leaveAnim
        NumberAnimation {
            id: leaveWidthAnim
            target: body
            property: "width"
            to: root.width
            duration: 125
        }
        NumberAnimation {
            id: leaveHeightAnim
            target: body
            property: "height"
            to: root.height
            duration: 125
        }
    }
    function startLeaveAnim() {
        leaveWidthAnim.from = body.width
        leaveHeightAnim.from = body.height
        leaveAnim.start()
    }

    SlantedBackground {
        id: body
        anchors.centerIn: parent
        width: parent.width // Note: This binding gets broken upon anim activation
        height: parent.height // Same
        radius: root.cornerRadius
        leftPartSkewDegrees: root.leftBodyPartSlantDegrees
        rightPartSkewDegrees: root.rightBodyPartSlantDegrees
        shadowOpacity: Math.min(1.0, width / parent.width)
        enabled: root.enabled

        transform: Matrix4x4 { matrix: translationMatrix }

        fillColor: !root.enabled ? "darkgrey" : (hasActiveHighlight ? Material.accentColor : Qt.lighter(Material.backgroundColor, 1.35))
        opacity: !root.enabled ? 0.2 : 1.0

        Behavior on fillColor { ColorAnimation { duration: highlightAnim.colorAnimDuration } }
    }

    Component {
        id: placeholderComponent
        Rectangle {
            width: implicitWidth
            height: implicitHeight
            implicitWidth: 12 + root._iconFlashExtraWidthAndHeight
            implicitHeight: 12 + root._iconFlashExtraWidthAndHeight
            radius: 1
            opacity: root.enabled ? (root.hasActiveHighlight ? 1.0 : 0.7) : 0.33
            transform: Matrix4x4 { matrix: UI.ui.makeSkewXMatrix(height, textSlantDegrees).times(translationMatrix) }
        }
    }

    Component {
        id: iconComponent
        Item {
            width: implicitWidth
            height: implicitHeight
            implicitWidth: root.iconWidthAndHeight
            implicitHeight: root.iconWidthAndHeight

            Image {
                id: icon
                visible: !root.hasActiveHighlight
                anchors.centerIn: parent
                width: root.iconWidthAndHeight + root._iconFlashExtraWidthAndHeight
                height: root.iconWidthAndHeight + root._iconFlashExtraWidthAndHeight
                smooth: true
                mipmap: true
                source: root.iconPath
            }

            Loader {
                anchors.centerIn: parent
                active: icon.status === Image.Error
                sourceComponent: placeholderComponent
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
        active: iconPath.length > 0 || root.displayIconPlaceholder
        anchors.right: label.left
        anchors.verticalCenter: parent.verticalCenter
        anchors.rightMargin: 6
        width: iconOrPlaceholder.item ? iconOrPlaceholder.item.implicitWidth : 0
        height: iconOrPlaceholder.item ? iconOrPlaceholder.item.implicitHeight : 0
        sourceComponent: iconPath.length > 0 ? iconComponent : placeholderComponent
    }

    UIFlashLabel {
        id: label

        anchors.centerIn: parent
        anchors.horizontalCenterOffset: parent.labelHorizontalCenterOffset +
            (iconOrPlaceholder.item ? iconOrPlaceholder.width + iconOrPlaceholder.anchors.rightMargin : 0)

        text: root.text
        font.weight: Font.Black
        font.capitalization: Font.AllUppercase
        extraFontSpacingOnHover: 0.5
        // Note: These calculations are only valid for centered flash
        maxFlashWidthOvershoot: 0.5 * root.extraLabelWidthOnFlash
        maxFlashHeightOvershoot: 0.5 * root.extraLabelHeightOnFlash

        transform: Matrix4x4 { matrix: UI.ui.makeSkewXMatrix(label.height, textSlantDegrees).times(translationMatrix) }
        opacity: root.enabled ? 1.0 : 0.5
    }
}