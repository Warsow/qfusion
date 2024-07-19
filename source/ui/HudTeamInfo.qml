import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root
    width: implicitWidth
    height: implicitHeight
    
    property var commonDataModel
    property var povDataModel

    implicitWidth: Hud.valueBarWidth
    implicitHeight: list.height + 2 * Hud.elementRadius

    readonly property color evenRowColor: Qt.rgba(0.0, 0.0, 0.0, 0.7)
    readonly property color oddRowColor: Qt.rgba(0.08, 0.08, 0.08, 0.7)

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: {
            if (list.count) {
                Hud.ui.supplyDisplayedHudItemAndMargin(back, 16.0)
            }
        }
    }

    Rectangle {
        id: back
        visible: list.count
        anchors.centerIn: parent
        width: Hud.valueBarWidth
        height: list.height + 2 * Hud.elementRadius
        radius: Hud.elementRadius
        color: "transparent"
        border.color: Qt.rgba(0.0, 0.0, 0.0, 0.7)
        border.width: Hud.elementRadius
        layer.enabled: list.count
        layer.effect: ElevationEffect { elevation: Hud.elementElevation }
        Component.onDestruction: Hud.destroyLayer(layer)
    }

    ListView {
        id: list
        anchors.centerIn: parent
        height: contentHeight
        width: Hud.valueBarWidth - 2 * Hud.elementRadius
        model: root.povDataModel.getTeamListModel()
        delegate: Rectangle {
            id: listDelegate
            width: list.width
            implicitHeight: 40
            color: index % 2 ? oddRowColor : evenRowColor
            Component.onDestruction: {
                Hud.ui.ensureObjectDestruction(model)
                Hud.ui.ensureObjectDestruction(listDelegate)
            }

            RowLayout {
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.leftMargin: 6
                anchors.rightMargin: 6
                anchors.verticalCenter: parent.verticalCenter
                spacing: 12

                Image {
                    Layout.preferredWidth: 20
                    Layout.preferredHeight: 20
                    smooth: true
                    mipmap: true
                    source: weaponIconPath
                }
                HudLabel {
                    Layout.preferredWidth: 32
                    horizontalAlignment: Qt.AlignRight
                    font.family: Hud.ui.numbersFontFamily
                    font.weight: Font.Bold
                    textFormat: Text.PlainText
                    text: health
                }
                HudLabel {
                    font.family: Hud.ui.numbersFontFamily
                    font.weight: Font.Bold
                    textFormat: Text.PlainText
                    text: "|"
                }
                HudLabel {
                    Layout.preferredWidth: 32
                    horizontalAlignment: Qt.AlignLeft
                    font.family: Hud.ui.numbersFontFamily
                    font.weight: Font.Bold
                    textFormat: Text.PlainText
                    text: armor
                }
                Column {
                    Layout.preferredWidth: Hud.tinyValueBarWidth
                    Layout.leftMargin: 4
                    Layout.rightMargin: 4
                    spacing: 2 * Hud.tinyValueBarMargin
                    Rectangle {
                        width: Hud.tinyValueBarWidth
                        height: Hud.tinyValueBarHeight
                        color: Qt.rgba(1.0, 1.0, 1.0, 0.15)
                        radius: Hud.tinyValueBarRadius
                        Rectangle {
                            width: parent.width * 0.01 * Math.min(Math.max(0, health), 100)
                            height: parent.height
                            radius: Hud.tinyValueBarRadius
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            color: health > 100 ? "deeppink" : "green"
                        }
                    }
                    Rectangle {
                        width: Hud.tinyValueBarWidth
                        height: Hud.tinyValueBarHeight
                        color: Qt.rgba(1.0, 1.0, 1.0, 0.15)
                        radius: Hud.tinyValueBarRadius
                        Rectangle {
                            width: parent.width * 0.01 * Math.min(Math.max(0, armor), 100)
                            height: parent.height
                            radius: Hud.tinyValueBarRadius
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            color: "white"
                        }
                    }
                }
                HudLabel {
                    Layout.fillWidth: true
                    Layout.preferredWidth: 1.00
                    horizontalAlignment: Qt.AlignHCenter
                    font.weight: Font.Bold
                    font.pointSize: Hud.labelFontSize - 0.5 // wtf? Also do all systems support fractional size?
                    textFormat: Text.StyledText
                    text: nickname
                    maximumLineCount: 1
                    elide: Text.ElideMiddle
                }
            }
        }
    }
}