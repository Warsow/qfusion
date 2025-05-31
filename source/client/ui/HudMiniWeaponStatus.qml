import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import net.warsow 2.6

Item {
    id: root
    width: implicitWidth
    height: implicitHeight
    implicitWidth: back.width + 2 * back.border.width
    implicitHeight: back.height + 2 * back.border.width

    property var povDataModel
    property real miniviewScale
    property int displayMode

    readonly property var actualScale: textMetrics.boundingRect.height / 80

    TextMetrics {
        id: textMetrics
        font.family: Hud.ui.numbersFontFamily
        font.pointSize: Math.max(32 * miniviewScale, 12)
        text: "0"
    }

    Rectangle {
        id: back
        anchors.centerIn: parent
        radius: 12 * actualScale
        border.width: 2
        // These properties below are overridden by the "compact" state
        color: Hud.elementBackgroundColor
        border.color: Hud.elementBackgroundColor
        width: Hud.miniviewItemWidth * actualScale
        height: Hud.miniviewItemHeight * actualScale
    }

    Image {
        id: icon
        anchors.top: undefined
        anchors.right: back.right
        anchors.bottom: back.bottom
        anchors.margins: 8 * actualScale
        smooth: true
        mipmap: true
        source: povDataModel.activeWeaponIcon
        Behavior on width { NumberAnimation { duration: 50 } }
        Behavior on height { NumberAnimation { duration: 50 } }
    }

    readonly property real strongAmmo: povDataModel.activeWeaponStrongAmmo
    readonly property real weakAmmo: povDataModel.activeWeaponWeakAmmo
    readonly property real primaryAmmo: strongAmmo || weakAmmo

    // Note: See HudMiniValueBar, looks like we can't rely on restoring default state (wtf?)
    states: [
        State {
            name: "compact"
            when: displayMode === Hud.DisplayMode.Compact
            PropertyChanges {
                target: icon
                width: 20
                height: 20
            }
            AnchorChanges {
                target: icon
                anchors.top: undefined
                anchors.right: undefined
                anchors.horizontalCenter: back.horizontalCenter
                anchors.verticalCenter: back.verticalCenter
                anchors.bottom: undefined
            }
            PropertyChanges {
                target: back
                width: 96 * actualScale
                height: 96 * actualScale
                border.color: povDataModel.activeWeaponColor
            }
            PropertyChanges {
                target: primaryLabel
                visible: false
            }
            PropertyChanges {
                target: secondaryLabel
                visible: false
            }
        },
        State {
            name: "regular"
            when: displayMode === Hud.DisplayMode.Regular
            PropertyChanges {
                target: icon
                width: 80 * actualScale
                height: 80 * actualScale
            }
            PropertyChanges {
                target: primaryLabel
                visible: true
                font.pointSize: (primaryAmmo >= 0 ? 32 : 40) * (1.75 * actualScale)
                anchors.bottomMargin: (primaryAmmo >= 0 ? -4 : -20) * (1.75 * actualScale)
            }
            PropertyChanges {
                target: secondaryLabel
                visible: true
                font.pointSize: ((strongAmmo && weakAmmo) ? (weakAmmo >= 0 ? 16 : 24) : 14) * (1.75 * actualScale)
            }
        },
        State {
            name: "extended"
            when: displayMode === Hud.DisplayMode.Extended
            PropertyChanges {
                target: primaryLabel
                visible: true
                font.pointSize: (primaryAmmo >= 0 ? 32 : 40) * (1.25 * actualScale)
                anchors.bottomMargin: (primaryAmmo >= 0 ? -4 : -20) * (1.25 * actualScale)
            }
            PropertyChanges {
                target: secondaryLabel
                visible: true
                font.pointSize: ((strongAmmo && weakAmmo) ? (weakAmmo >= 0 ? 16 : 24) : 14) * (1.25 * actualScale)
            }
            PropertyChanges {
                target: icon
                width: 128 * actualScale
                height: 128 * actualScale
            }
        }
    ]

    Text {
        id: primaryLabel
        font.weight: Font.Black
        font.letterSpacing: 1.25
        anchors.left: secondaryLabel.left
        anchors.bottom: secondaryLabel.top
        style: Text.Raised
        textFormat: Text.PlainText
        color: Material.foreground
        text: (primaryAmmo >= 0) ? primaryAmmo : Hud.infinityString
    }

    Text {
        id: secondaryLabel
        anchors.left: back.left
        anchors.bottom: back.bottom
        anchors.leftMargin: 1 + 15 * actualScale
        anchors.bottomMargin: 1 + 7 * actualScale
        font.family: Hud.ui.numbersFontFamily
        font.weight: Font.Black
        font.letterSpacing: 1.25
        style: Text.Raised
        textFormat: Text.PlainText
        color: primaryAmmo ? Material.foreground : "red"
        opacity: (strongAmmo && weakAmmo) ? 1.0 : 0.5
        text: (strongAmmo && weakAmmo) ? ("+" + (weakAmmo > 0 ? weakAmmo : Hud.infinityString)) :
            (strongAmmo ? "STRNG" : (weakAmmo ? "WEAK" : "OVER"))
    }
}