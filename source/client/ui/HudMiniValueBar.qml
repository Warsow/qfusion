import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import net.warsow 2.6

Item {
    id: root
    width: implicitWidth
    height: implicitHeight
    implicitWidth: back.width
    implicitHeight: back.height

    property string iconPath
    property string text
    property color color
    property real value
    property real frac
    property real miniviewScale
    property int displayMode
    property string debugTag

    // The actual scale is limited by the label (except the compact mode)
    readonly property real actualScale: Math.max(72 * miniviewScale, valueLabel.implicitHeight + 2) / 72.0
    readonly property real commonMargin: 3 + 5 * actualScale

    states: [
        State {
            name: "compact"
            when: displayMode === Hud.DisplayMode.Compact
            PropertyChanges {
                target: back
                width: Hud.tinyValueBarWidth + 2.0 * Hud.tinyValueBarMargin + 2.0 // see the equation for the inner bar width
                height: Hud.tinyValueBarHeight + 2.0 * Hud.tinyValueBarMargin
                radius: Hud.tinyValueBarRadius
            }
            PropertyChanges {
                target: bar
                height: Hud.tinyValueBarHeight
                anchors.margins: Hud.tinyValueBarMargin
            }
            PropertyChanges {
                target: innerBar
                height: Hud.tinyValueBarHeight
            }
            PropertyChanges {
                target: valueLabel
                visible: false
            }
            PropertyChanges {
                target: image
                visible: false
                source: ""
            }
        },
        // TODO: This is a workaround for the default state not being rolled back (wtf?)
        State {
            name: "normal"
            when: displayMode !== Hud.DisplayMode.Compact
            PropertyChanges {
                target: back
                width: 0.7 * Hud.valueBarWidth * actualScale
                height: 108 * actualScale
                radius: 1 + 3 * actualScale
            }
            PropertyChanges {
                target: bar
                height: 24 * actualScale
                anchors.margins: 8 * miniviewScale
            }
            PropertyChanges {
                target: innerBar
                height: bar.height - 2
            }
            PropertyChanges {
                target: valueLabel
                visible: true
            }
            PropertyChanges {
                target: image
                visible: true
                source: iconPath
            }
        }
    ]

    Rectangle {
        id: back
        anchors.centerIn: parent
        color: Hud.elementBackgroundColor
    }

    Image {
        id: image
        anchors.left: back.left
        anchors.verticalCenter: back.verticalCenter
        anchors.verticalCenterOffset: -0.5 * bar.height
        anchors.leftMargin: commonMargin
        width: 40 * actualScale
        height: 40 * actualScale
        smooth: true
        mipmap: true
    }

    Item {
        id: bar
        width: back.width
        anchors.left: back.left
        anchors.right: back.right
        anchors.bottom: back.bottom
        // Prevent the animated value bar to leave the desired bounds
        clip: displayMode === Hud.DisplayMode.Extended

        Rectangle {
            anchors.fill: parent
            color: Qt.tint("black", Qt.rgba(root.color.r, root.color.g, root.color.b, 0.2))
            radius: back.radius
        }

        Rectangle {
            id: innerBar
            width: Math.max(parent.width * root.frac - 2, 0)
            anchors.left: parent.left
            anchors.verticalCenter: parent.verticalCenter
            radius: back.radius
            color: root.color

            Behavior on width {
                enabled: displayMode === Hud.DisplayMode.Extended
                NumberAnimation {
                    duration: 125
                    easing.type: Easing.OutElastic
                }
            }
        }
    }

    Text {
        id: valueLabel
        visible: true
        width: 96 * actualScale
        anchors.right: back.right
        anchors.rightMargin: commonMargin
        anchors.verticalCenter: back.verticalCenter
        anchors.verticalCenterOffset: -0.5 * bar.height
        horizontalAlignment: Qt.AlignRight
        font.family: Hud.ui.numbersFontFamily
        // This condition limits the mininal height of the entire HUD element
        font.pointSize: Math.max(28 * miniviewScale, 10)
        font.weight: Font.Black
        font.letterSpacing: 1.25
        textFormat: Text.PlainText
        text: root.value
        color: Material.foreground
        style: Text.Raised
    }
}