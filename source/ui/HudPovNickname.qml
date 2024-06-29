import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    property var povDataModel
    property bool isMiniview
    property bool applyOutline
    property real miniviewScale

    implicitWidth: row.width + 32
    implicitHeight: isMiniview ? back.height : 56

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: {
            if (!isMiniview) {
                Hud.ui.supplyDisplayedHudItemAndMargin(root, 1.0)
            }
        }
    }

    Rectangle {
        id: back
        visible: isMiniview
        anchors.centerIn: root
        width: row.width + 32
        height: 32
        color: Qt.rgba(0.0, 0.0, 0.0, 0.7)
        border.color: applyOutline ? Qt.rgba(1.0, 1.0, 1.0, 0.5) : Qt.rgba(0.0, 0.0, 0.0, 0.7)
        border.width: applyOutline ? 2 : 0
        radius: 3
    }

    RowLayout {
        id: row
        anchors.centerIn: root
        spacing: 8
        Image {
            source: "image://wsw/gfx/hud/cam"
            Layout.preferredWidth: isMiniview ? 16 : 32
            Layout.preferredHeight: isMiniview ? 16 : 32
            Layout.alignment: Qt.AlignVCenter
            smooth: true
            mipmap: true
            opacity: isMiniview ? 0.5 : 0.85
        }
        HudLabel {
            Layout.preferredWidth: implicitWidth
            Layout.preferredHeight: implicitHeight
            Layout.alignment: Qt.AlignVCenter
            text: povDataModel.nickname
            font.family: isMiniview ? Hud.ui.regularFontFamily : Hud.ui.headingFontFamily
            font.weight: isMiniview ? Font.Bold : Font.ExtraBold
            font.pointSize: isMiniview ? Hud.labelFontSize : 20
            font.letterSpacing: 1
            style: Text.Raised
            opacity: isMiniview ? 1.0 : 0.85
        }
    }
}