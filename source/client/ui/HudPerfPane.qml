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
    implicitHeight: (showFps && showNet) ? Hud.miniviewItemHeight : ((showFps || showNet) ? Hud.teamScoreHeight : 0.0)

    readonly property bool showFps: !!(Hud.commonDataModel.activeItemsMask & HudLayoutModel.ShowFps)
    readonly property bool showNet: !!(Hud.commonDataModel.activeItemsMask & HudLayoutModel.ShowNet)

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: {
            if (Hud.commonDataModel.activeItemsMask) {
                Hud.ui.supplyDisplayedHudItemAndMargin(root, 1.0)
            }
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
            visible: root.showFps
            Layout.fillWidth: true
            Layout.fillHeight: true
            title: "FPS"
            altTitle: "FRME<br>TIME"
            primaryValueText: Math.round(1000 / rowDataModel.average)
            fixedVisualMin: 0.0
            maxVisualFrac: showNet ? 0.9 : 1.0
            useFixedLevelIfSteady: true
            displayLowerBar: true
            rowDataModel: Hud.commonDataModel.getFrametimeDataRowModel()
            strokeColor: Hud.ui.colorWithAlpha("white", 0.7)
            valueFormatter: v => '' + Math.round(v)
        }
        HudPerfRow {
            visible: root.showNet
            Layout.fillWidth: true
            Layout.fillHeight: true
            title: "PING"
            primaryValueText: Math.round(rowDataModel.average)
            fixedVisualMin: 0.0
            useFixedLevelIfSteady: true
            steadyVisualFrac: 0.75
            rowDataModel: Hud.commonDataModel.getPingDataRowModel()
            strokeColor: Hud.ui.colorWithAlpha("white", 0.7)
            HudPerfRow {
                anchors.fill: parent
                fixedVisualMin: 0.0
                fixedVisualMax: 1.0
                altTitle: "<font color=\"red\">PCKT<br>LOSS</font>"
                rowDataModel: Hud.commonDataModel.getPacketlossDataRowModel()
                valueFormatter: v => ''
                strokeColor: Hud.ui.colorWithAlpha("orangered", 0.7)
            }
        }
    }
}