import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root
    implicitWidth: cardWidth * povDataModel.getInventoryModel().numInventoryItems +
                       layout.spacing * (povDataModel.getInventoryModel().numInventoryItems - 1)
    implicitHeight: cardHeight
    width: implicitWidth
    height: implicitHeight

    property var povDataModel

    readonly property real cardWidth: 64
    readonly property real cardHeight: 108
    readonly property real cardRadius: 1.2 * Hud.elementRadius

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: Hud.ui.supplyDisplayedHudItemAndMargin(layout, 24.0)
    }

    RowLayout {
        id: layout
        anchors.centerIn: parent
        spacing: Hud.elementMargin

        Repeater {
            model: root.povDataModel.getInventoryModel()
            delegate: Item {
                id: delegateItem
                implicitWidth: visible ? cardWidth : 0
                implicitHeight: visible ? cardHeight : 0
                Layout.preferredWidth: implicitWidth
                Layout.preferredHeight: implicitHeight
                Layout.alignment: Qt.AlignVCenter
                visible: model.displayed

                Behavior on implicitWidth {
                    SmoothedAnimation { duration: 100 }
                }
                Behavior on implicitHeight {
                    SmoothedAnimation { duration: 100 }
                }

                Component.onDestruction: {
                    Hud.ui.ensureObjectDestruction(model)
                    Hud.ui.ensureObjectDestruction(delegateItem)
                }

                Rectangle {
                    id: frame
                    anchors.centerIn: parent
                    width: delegateItem.width
                    height: cardHeight
                    radius: cardRadius
                    color: Hud.elementBackgroundColor
                    layer.effect: ElevationEffect { elevation: Hud.elementElevation }
                    Component.onDestruction: Hud.destroyLayer(layer)
                }

                Rectangle {
                    anchors.centerIn: parent
                    width: delegateItem.visible ? delegateItem.width + 3 : 0
                    height: cardHeight + 3
                    radius: cardRadius - 2
                    color: "transparent"
                    border.width: 4
                    border.color: Qt.lighter(model.color, 1.1)
                    visible: active
                }

                Image {
                    id: icon
                    visible: delegateItem.width + 2 > icon.width
                    anchors.centerIn: parent
                    width: 32
                    height: 32
                    source: model.hasWeapon ? model.iconPath : (model.iconPath + "?grayscale=true")
                    smooth: true
                    mipmap: true
                }

                Label {
                    anchors.top: frame.top
                    visible: delegateItem.width > 24
                    anchors.topMargin: strongAmmoCount >= 0 ? 6 : 4
                    anchors.horizontalCenter: parent.horizontalCenter
                    font.family: Hud.ui.numbersFontFamily
                    font.weight: Font.Black
                    font.pointSize: strongAmmoCount >= 0 ? 15 : 18
                    font.letterSpacing: 1.0
                    opacity: strongAmmoCount ? 1.0 : 0.5
                    textFormat: Text.PlainText
                    text: strongAmmoCount >= 0 ? (strongAmmoCount ? strongAmmoCount : Hud.missingString) : Hud.infinityString
                    style: Text.Raised
                }

                Label {
                    anchors.bottom: frame.bottom
                    visible: delegateItem.width > 24
                    anchors.bottomMargin: weakAmmoCount >= 0 ? 6 : 4
                    anchors.horizontalCenter: parent.horizontalCenter
                    font.family: Hud.ui.numbersFontFamily
                    font.weight: Font.Black
                    font.pointSize: weakAmmoCount >= 0 ? 15 : 18
                    font.letterSpacing: 1.0
                    opacity: weakAmmoCount ? 1.0 : 0.5
                    textFormat: Text.PlainText
                    text: weakAmmoCount >= 0 ? (weakAmmoCount ? weakAmmoCount : Hud.missingString) : Hud.infinityString
                    style: Text.Raised
                }
            }
        }
    }
}