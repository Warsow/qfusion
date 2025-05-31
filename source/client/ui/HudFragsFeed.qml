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

    implicitWidth: 256
    implicitHeight: list.contentHeight

    property var commonDataModel

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: {
            if (list.count) {
                Hud.ui.supplyDisplayedHudItemAndMargin(list, 4.0)
            }
        }
    }

    ListView {
        id: list
        model: root.commonDataModel.getFragsFeedModel()
        width: parent.width
        height: contentHeight
        anchors.centerIn: parent
        spacing: 4

        delegate: RowLayout {
            id: listDelegate
            width: list.width
            height: 27
            // Something is wrong with transitions (the don't always complete, wtf?). Use a simpler approach.
            opacity: 0.0
            Component.onCompleted: opacity = 1.0
            Component.onDestruction: Hud.ui.ensureObjectDestruction(listDelegate)
            Behavior on opacity { NumberAnimation { duration: 200 } }
            HudLabel {
                Layout.fillWidth: true
                Layout.alignment: Qt.AlignVCenter
                horizontalAlignment: Qt.AlignRight
                font.weight: Font.Bold
                font.letterSpacing: 1.25
                font.pointSize: 15
                style: Text.Raised
                textFormat: Text.StyledText
                text: attacker
            }
            Image {
                Layout.preferredWidth: 20
                Layout.preferredHeight: 20
                Layout.alignment: Qt.AlignVCenter
                source: iconPath
                smooth: true
                mipmap: true
            }
            HudLabel {
                Layout.alignment: Qt.AlignVCenter
                font.weight: Font.Bold
                font.letterSpacing: 1.25
                font.pointSize: 15
                style: Text.Raised
                textFormat: Text.StyledText
                text: victim
            }
        }
    }
}