pragma Singleton
import QtQuick 2.12
import net.warsow 2.6

QtObject {
    readonly property var ui: __ui
    readonly property var serverListModel: __serverListModel
    readonly property var hudCommonDataModel: __hudCommonDataModel
    readonly property var hudPovDataModel: __hudPovDataModel
    readonly property var regularHudEditorModel: __regularHudEditorModel
    readonly property var miniviewHudEditorModel: __miniviewHudEditorModel
    readonly property var keysAndBindings: __keysAndBindings
    readonly property var demosModel: __demosModel
    readonly property var demosResolver: __demosResolver
    readonly property var demoPlayer: __demoPlayer
    readonly property var gametypesModel: __gametypesModel
    readonly property var gametypeOptionsModel: __gametypeOptionsModel
    readonly property var regularCallvotesModel: __regularCallvotesModel
    readonly property var operatorCallvotesModel: __operatorCallvotesModel
    readonly property var chatProxy: __chatProxy
    readonly property var teamChatProxy: __teamChatProxy
    readonly property var playersModel: __playersModel
    readonly property var scoreboard: __scoreboard
    readonly property var scoreboardSpecsModel: __scoreboardSpecsModel
    readonly property var scoreboardPlayersModel: __scoreboardPlayersModel
    readonly property var scoreboardAlphaModel: __scoreboardAlphaModel
    readonly property var scoreboardBetaModel: __scoreboardBetaModel
    readonly property var scoreboardMixedModel: __scoreboardMixedModel

    readonly property real tabHeight: 52.0

    // Make sure that all these labels have the fixed height for consistency reasons
    // TODO: Use metrics
    // Note: The title margin is slightly lower to account for header vertical-center alignment
    readonly property real titleLabelHeight: 16
    readonly property real titleLabelTopMargin: 16
    readonly property real descLabelHeight: 16
    readonly property real descLabelTopMargin: 24

    readonly property real logoDecorationRowHeight: 11
    readonly property real logoDecorationRowSpacing: 12

    readonly property real regularSlantedButtonHeight: 42.0
    readonly property real buttonBodySlantDegrees: 20.0
    readonly property real buttonTextSlantDegrees: 15.0
    readonly property real neutralCentralButtonWidth: 144 + 16 + 8
    readonly property real acceptOrRejectButtonWidth: 128 + 16
    readonly property real minAcceptRejectSpacing: 48

    readonly property real acceptRejectRowWidth: 512 + 32
    readonly property real acceptRejectRowHeight: tabHeight
    readonly property real acceptRejectRowBottomMargin: 24

    readonly property real desiredPopupContentWidth: 420.0

    readonly property real demoCompactRowHeight: 56.0
    readonly property real demoWideRowHeight: 22

    readonly property real settingsRowHeight: 44
    readonly property real settingsRowSpacing: 48

    readonly property real labelFontSize: 12
    readonly property real labelLetterSpacing: 0.75
    readonly property int labelFontWeight: Font.Normal

    readonly property real scoreboardFontSize: 13
    readonly property real scoreboardLetterSpacing: 0.75

    readonly property real fullscreenOverlayOpacity: 0.90

    readonly property real mainMenuButtonWidth: 224.0
    readonly property real mainMenuButtonTrailWidth: 1.5 * 224.0

    readonly property real boldLineHeight: 3.0

    function destroyLayer(layer) {
        if (layer) {
            layer.enabled = false
            layer.effect  = null
            ui.ensureObjectDestruction(layer)
        }
    }
}