import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import net.warsow 2.6

Item {
    id: root
    width: implicitWidth
    height: implicitHeight
    implicitWidth: Hud.miniviewItemWidth + Hud.miniviewBorderWidth
    implicitHeight: Hud.miniviewItemHeight + Hud.miniviewBorderWidth
    property var povDataModel

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: Hud.ui.supplyDisplayedHudItemAndMargin(back, 16.0)
    }

    Rectangle {
        id: back
        anchors.fill: parent
        radius: Hud.elementRadius
        color: Hud.elementBackgroundColor
        layer.enabled: true
        layer.effect: ElevationEffect { elevation: Hud.elementElevation }
        Component.onDestruction: Hud.destroyLayer(layer)
    }

    Label {
        id: weaponNameLabel
        anchors.top: back.top
        anchors.topMargin: 12
        anchors.horizontalCenter: back.horizontalCenter
        font.family: Hud.ui.headingFontFamily
        font.weight: Font.Black
        font.letterSpacing: 1.25
        font.pointSize: 14
        font.capitalization: Font.AllUppercase
        textFormat: Text.PlainText
        text: povDataModel.activeWeaponName
    }

    Image {
        anchors.right: back.right
        anchors.bottom: back.bottom
        anchors.margins: 8
        width: 96
        height: 96
        smooth: true
        mipmap: true
        source: povDataModel.activeWeaponIcon
    }

    readonly property real strongAmmo: povDataModel.activeWeaponStrongAmmo
    readonly property real weakAmmo: povDataModel.activeWeaponWeakAmmo
    readonly property real primaryAmmo: strongAmmo || weakAmmo

    readonly property string infinity: "\u221E"

    Label {
        id: secondaryCountLabel
        // Display as a secondary if there's strong ammo as well
        visible: strongAmmo && weakAmmo
        anchors.left: primaryCountLabel.left
        anchors.bottom: primaryCountLabel.top
        anchors.bottomMargin: weakAmmo >= 0 ? -4 : -24
        font.family: Hud.ui.numbersFontFamily
        font.weight: Font.Black
        font.letterSpacing: 1.25
        font.pointSize: weakAmmo >= 0 ? 16 : 24
        style: Text.Raised
        textFormat: Text.PlainText
        text: weakAmmo > 0 ? weakAmmo : infinity
    }

    Label {
        id: primaryCountLabel
        anchors.left: back.left
        anchors.bottom: back.bottom
        anchors.leftMargin: 16
        anchors.bottomMargin: primaryAmmo >= 0 ? 16 : 6
        font.weight: Font.Black
        font.letterSpacing: 1.25
        font.pointSize: primaryAmmo >= 0 ? 32 : 40
        style: Text.Raised
        textFormat: Text.PlainText
        text: (primaryAmmo >= 0) ? primaryAmmo : infinity
    }
}