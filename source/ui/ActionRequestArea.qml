import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

ListView {
    id: root
    spacing: Hud.elementMargin
    model: Hud.actionRequestsModel

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: {
            if (root.count) {
                Hud.ui.supplyDisplayedHudItemAndMargin(root, 64.0)
            }
        }
    }

    delegate: Item {
        id: listDelegate
        Component.onDestruction: Hud.ui.ensureObjectDestruction(listDelegate)
        width: parent.width
        height: Math.max(96, contentColumn.implicitHeight)
        Rectangle {
            anchors.fill: parent
            radius: 3
            color: expectsInput ? Material.accent : Qt.lighter(Material.background)
            opacity: 0.8
            layer.enabled: true
            layer.effect: ElevationEffect { elevation: Hud.elementElevation }
            Component.onDestruction: Hud.destroyLayer(layer)
        }

        ColumnLayout {
            id: contentColumn
            anchors.centerIn: parent
            width: parent.width
            // Caution, don't use UILabel as it depends of the UI singleton which should not be used in the HUD Qml sandbox
            Label {
                Layout.fillWidth: true
                Layout.leftMargin: 12
                Layout.rightMargin: 12
                horizontalAlignment: Qt.AlignHCenter
                verticalAlignment: Qt.AlignVCenter
                wrapMode: Text.WordWrap
                font.weight: Font.Bold
                font.pointSize: Hud.labelFontSize + 2.0
                font.letterSpacing: Hud.labelLetterSpacing
                textFormat: Text.StyledText
                text: title
            }
            Label {
                visible: desc.length > 0
                Layout.fillWidth: true
                Layout.leftMargin: 12
                Layout.rightMargin: 12
                horizontalAlignment: Qt.AlignHCenter
                verticalAlignment: Qt.AlignVCenter
                wrapMode: Text.WordWrap
                font.weight: Font.Bold
                font.pointSize: Hud.labelFontSize + 1.0
                font.letterSpacing: Hud.labelLetterSpacing
                textFormat: Text.StyledText
                text: desc
            }
        }
    }
}