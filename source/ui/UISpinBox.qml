import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import net.warsow 2.6

SpinBox {
    id: root

    Material.theme: activeFocus ? Material.Light : Material.Dark

    implicitWidth: 144
    implicitHeight: 36
    padding: 0
    topPadding: 0
    bottomPadding: 0
    leftPadding: 0
    rightPadding: 0
    leftInset: 0
    rightInset: 0
    topInset: 0
    bottomInset: 0

    readonly property real indicatorSide: 24
    readonly property real indicatorMargin: 5

    onValueModified: {
        UI.ui.playSwitchSound()
    }

    background: Rectangle {
        color: UI.ui.colorWithAlpha(Material.background, 0.67)
    }

    contentItem: UILabel {
        text: root.displayText
        horizontalAlignment: Qt.AlignHCenter
        verticalAlignment: Qt.AlignVCenter
    }

    down.indicator: Item {
        x: indicatorMargin
        y: (root.height - indicatorSide) / 2
        width: indicatorSide
        height: indicatorSide

        Label {
            width: indicatorSide
            height: indicatorSide
            horizontalAlignment: Qt.AlignHCenter
            verticalAlignment: Qt.AlignVCenter
            font.family: UI.ui.symbolsFontFamily
            font.pointSize: 8
            text: "\u25C0"
        }

        Ripple {
            width: indicatorSide
            height: indicatorSide
            pressed: root.down.pressed
            active: root.down.pressed || root.down.hovered
            color: Material.rippleColor
        }
    }

    up.indicator: Item {
        x: root.width - width - indicatorMargin
        y: (root.height - indicatorSide) / 2

        width: indicatorSide
        height: indicatorSide

        Label {
            width: indicatorSide
            height: indicatorSide
            horizontalAlignment: Qt.AlignHCenter
            verticalAlignment: Qt.AlignVCenter
            font.family: UI.ui.symbolsFontFamily
            font.pointSize: 8
            text: "\u25B6"
        }

        Ripple {
            width: indicatorSide
            height: indicatorSide
            pressed: root.up.pressed
            active: root.up.pressed || root.up.hovered
            color: Material.rippleColor
        }
    }
}
