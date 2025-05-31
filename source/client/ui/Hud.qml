pragma Singleton
import QtQuick 2.12
import net.warsow 2.6

QtObject {
    readonly property var ui: __ui
    readonly property var commonDataModel: __hudCommonDataModel
    readonly property var povDataModel: __hudPovDataModel
    readonly property var actionRequestsModel: __actionRequestsModel
    readonly property var chatProxy: __chatProxy
    readonly property var teamChatProxy: __teamChatProxy

    enum DisplayMode { Compact, Regular, Extended }

    readonly property real elementMargin: 16
    readonly property real elementRadius: 6
    readonly property real elementElevation: 16
    readonly property color elementBackgroundColor: Qt.rgba(0.0, 0.0, 0.0, 0.7)
    readonly property real miniviewItemWidth: 240
    readonly property real miniviewItemHeight: 160
    readonly property real miniviewBorderWidth: 4
    readonly property real teamScoreHeight: 80
    readonly property real valueBarWidth: 256 + 64 + 32 + 16

    readonly property real tinyValueBarWidth: 32
    readonly property real tinyValueBarHeight: 5
    readonly property real tinyValueBarMargin: 2
    readonly property real tinyValueBarRadius: 1

    readonly property real labelFontSize: 13
    readonly property real labelLetterSpacing: 0.75
    readonly property int labelFontWeight: Font.Normal

    readonly property string infinityString: "\u221E"
    readonly property string missingString: "\u2013"

    function destroyLayer(layer) {
        if (layer) {
            layer.enabled = false
            layer.effect  = null
            ui.ensureObjectDestruction(layer)
        }
    }
}