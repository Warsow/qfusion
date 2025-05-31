import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    readonly property real modelSelectorWidth: 270
    readonly property real modelSelectorHeight: 400

    readonly property bool drawNativePart: StackView.view && !StackView.view.busy

    ColumnLayout {
        anchors.centerIn: parent

        CVarAwareCheckBox {
            Layout.alignment: Qt.AlignHCenter
            text: "Force my team to always look like team <b>ALPHA</b>"
            cvarName: "cg_forceMyTeamAlpha"
        }

        RowLayout {
            Layout.preferredWidth: root.width - 32
            Layout.topMargin: 36

            TeamSettingsTeamColumn {
                Layout.preferredWidth: root.modelSelectorWidth
                modelSelectorWidth: root.modelSelectorWidth
                modelSelectorHeight: root.modelSelectorHeight
                defaultModel: UI.ui.defaultTeamAlphaModel
                drawNativePart: root.drawNativePart
                teamName: "ALPHA"
            }

            Item { Layout.fillWidth: true }

            TeamSettingsTeamColumn {
                Layout.preferredWidth: root.modelSelectorWidth
                modelSelectorWidth: root.modelSelectorWidth
                modelSelectorHeight: root.modelSelectorHeight
                defaultModel: UI.ui.defaultTeamPlayersModel
                hasForceColorVar: true
                drawNativePart: root.drawNativePart
                teamName: "PLAYERS"
            }

            Item { Layout.fillWidth: true }

            TeamSettingsTeamColumn {
                Layout.preferredWidth: root.modelSelectorWidth
                modelSelectorWidth: root.modelSelectorWidth
                modelSelectorHeight: root.modelSelectorHeight
                defaultModel: UI.ui.defaultTeamBetaModel
                drawNativePart: root.drawNativePart
                teamName: "BETA"
            }
        }
    }
}
