import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root
    width: implicitWidth
    height: implicitHeight
    implicitWidth: Hud.miniviewItemWidth
    implicitHeight: Hud.miniviewItemHeight

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: {
            // TODO: Check if it's really displayed
            Hud.ui.supplyDisplayedHudItemAndMargin(root, 1.0)
        }
    }

    Rectangle {
        anchors.centerIn: parent
        width: parent.width
        height: parent.height
        radius: Hud.elementRadius
        color: Hud.elementBackgroundColor
    }

    ColumnLayout {
        anchors.centerIn: parent
        width: root.width - 2 * Hud.elementMargin
        height: root.height - 2 * Hud.elementMargin
        spacing: 8
        HudPerfRow {
            Layout.fillWidth: true
            Layout.fillHeight: true
            title: "FPS"
            altTitle: "FRME<br>TIME"
            primaryValueText: Math.round(1000 / Hud.commonDataModel.frametimeDataRow.average)
            rowData: Hud.commonDataModel.frametimeDataRow
            strokeColor: Hud.ui.colorWithAlpha("white", 0.7)
            valueFormatter: v => '' + Math.round(v)
        }
        HudPerfRow {
            Layout.fillWidth: true
            Layout.fillHeight: true
            title: "PING"
            primaryValueText: Math.round(Hud.commonDataModel.pingDataRow.average)
            fixedVisualMin: 0.0
            rowData: Hud.commonDataModel.pingDataRow
            strokeColor: Hud.ui.colorWithAlpha("white", 0.7)
            HudPerfRow {
                anchors.fill: parent
                fixedVisualMin: 0.0
                fixedVisualMax: 1.0
                altTitle: "<font color=\"red\">PCKT<br>LOSS</font>"
                rowData: Hud.commonDataModel.packetlossDataRow
                valueFormatter: v => ''
                strokeColor: Hud.ui.colorWithAlpha("orangered", 0.7)
            }
        }
    }
}