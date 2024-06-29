import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root
    width: implicitWidth
    height: implicitHeight
    implicitWidth: 720
    // Let it have intrinsic margins as it's special
    implicitHeight: list.height + 2 * Hud.elementMargin
    opacity: 0.9

    property var povDataModel

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: {
            if (list.count) {
                Hud.ui.supplyDisplayedHudItemAndMargin(list, 4.0)
            }
        }
    }

    Connections {
        target: root.povDataModel
        onIsMessageFeedFadingOutChanged: {
            if (isMessageFeedFadingOut) {
                fadeOutAnim.start()
            } else {
                fadeOutAnim.stop()
                opacity = 0.9
            }
        }
    }

    PropertyAnimation {
        id: fadeOutAnim
        target: root
        property: "opacity"
        to: 0.0
        duration: 2500
    }

    ListView {
        id: list
        anchors.top: parent.top
        anchors.topMargin: 10
        anchors.horizontalCenter: parent.horizontalCenter
        model: root.povDataModel.getMessageFeedModel()
        width: parent.width - 20
        height: contentHeight
        spacing: 3
        delegate: HudLabel {
            id: listDelegate
            width: list.width
            horizontalAlignment: Qt.AlignLeft
            elide: Text.ElideRight
            maximumLineCount: 1
            font.weight: Font.DemiBold
            textFormat: Text.StyledText
            style: Text.Raised
            text: message

            opacity: 0.0
            Component.onCompleted: opacity = 1.0
            Component.onDestruction: Hud.ui.ensureObjectDestruction(listDelegate)
            Behavior on opacity { NumberAnimation { duration: 100 } }
        }
    }
}