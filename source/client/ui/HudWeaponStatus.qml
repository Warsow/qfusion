import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import net.warsow 2.6

Item {
    id: root
    width: implicitWidth
    height: implicitHeight
    implicitWidth: Hud.miniviewItemWidth
    implicitHeight: Hud.miniviewItemHeight

    property var povDataModel
    property real lowAmmoAnimFrac

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

    readonly property real strongCount: povDataModel.activeWeaponStrongCount
    readonly property real weakCount: povDataModel.activeWeaponWeakCount
    readonly property real primaryCount: strongCount || weakCount
    readonly property bool hasSufficientPrimaryCount : strongCount ?
        (strongCount > povDataModel.activeWeaponStrongThreshold || strongCount < 0) :
        (weakCount > povDataModel.activeWeaponWeakThreshold || weakCount < 0)

    Label {
        id: primaryLabel
        font.weight: Font.Black
        font.letterSpacing: 1.25
        font.pointSize: primaryCount >= 0 ? 32 : 40
        anchors.left: secondaryLabel.left
        anchors.bottom: secondaryLabel.top
        anchors.bottomMargin: primaryCount >= 0 ? -4 : ((strongCount && weakCount) ? -22 : -12)
        style: Text.Raised
        textFormat: Text.PlainText
        color: hasSufficientPrimaryCount ? Material.foreground : "red"
        scale: hasSufficientPrimaryCount ? 1.0 : 1.0 + 0.03 * root.lowAmmoAnimFrac
        opacity: hasSufficientPrimaryCount ? 1.0 : (primaryCount ? (0.5 + 0.25 * root.lowAmmoAnimFrac) : 0.5)
        text: (primaryCount >= 0) ? primaryCount : Hud.infinityString
    }

    Label {
        id: secondaryLabel
        anchors.left: back.left
        anchors.bottom: back.bottom
        anchors.leftMargin: 16
        anchors.bottomMargin: 8
        font.family: Hud.ui.numbersFontFamily
        font.weight: Font.Black
        font.letterSpacing: 1.25
        font.pointSize: (strongCount && weakCount) ? (weakCount >= 0 ? 16 : 24) : 14
        style: Text.Raised
        textFormat: Text.PlainText
        color: primaryCount ? Material.foreground : "red"
        opacity: (strongCount && weakCount) ? 1.0 : 0.5
        text: (strongCount && weakCount) ? ("+" + (weakCount > 0 ? weakCount : Hud.infinityString)) :
            (strongCount ? "STRONG" : (weakCount ? "WEAK" : "OVER"))
    }
}