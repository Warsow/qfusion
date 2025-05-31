import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import net.warsow 2.6

Item {
    id: root
    implicitWidth: row.width
    // TODO: Avoid using magic numbers for the height of the team score element
    implicitHeight: Math.max(row.height, Hud.teamScoreHeight)

    property var commonDataModel

    // TODO: Use a single model (QAbstractItemModel) for all indicators?
    readonly property bool idle: !indicator1.indicatorProgress &&
        !indicator2.indicatorProgress && !indicator3.indicatorProgress

    readonly property int numEnabledIndicators:
        (root.commonDataModel.indicator1State.enabled ? 1 : 0) +
        (root.commonDataModel.indicator2State.enabled ? 1 : 0) +
        (root.commonDataModel.indicator3State.enabled ? 1 : 0)

    property real barHeightFrac: idle ? 0.0 : 1.0
    Behavior on barHeightFrac { SmoothedAnimation { duration: 500 } }

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: {
            if (numEnabledIndicators) {
                Hud.ui.supplyDisplayedHudItemAndMargin(root, 32.0)
            }
        }
    }

    Rectangle {
        anchors.fill: parent
        color: row.width ? Qt.rgba(0.0, 0.0, 0.0, 0.6) : "transparent"
        radius: Hud.elementRadius

        layer.enabled: row.width
        layer.effect: ElevationEffect { elevation: Hud.elementElevation }
        Component.onDestruction: Hud.destroyLayer(layer)
    }

    Row {
        id: row
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter

        HudObjectiveIndicator {
            id: indicator1
            useExclusiveMode: numEnabledIndicators === 1 && indicator1.indicatorState.enabled && indicator1.canUseExclusiveMode
            barHeightFrac: root.barHeightFrac
            indicatorState: root.commonDataModel.indicator1State
            commonDataModel: root.commonDataModel
        }
        HudObjectiveIndicator {
            id: indicator2
            useExclusiveMode: numEnabledIndicators === 1 && indicator2.indicatorState.enabled && indicator2.canUseExclusiveMode
            barHeightFrac: root.barHeightFrac
            indicatorState: root.commonDataModel.indicator2State
            commonDataModel: root.commonDataModel
        }
        HudObjectiveIndicator {
            id: indicator3
            useExclusiveMode: numEnabledIndicators === 1 && indicator3.indicatorState.enabled && indicator3.canUseExclusiveMode
            barHeightFrac: root.barHeightFrac
            indicatorState: root.commonDataModel.indicator3State
            commonDataModel: root.commonDataModel
        }
    }
}