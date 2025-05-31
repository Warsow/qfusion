import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Flickable {
    id: root
    flickableDirection: Flickable.VerticalFlick
    readonly property bool showGroupHeaders: rootItem.height > 800

    ColumnLayout {
        anchors.centerIn: parent
        width: 0.67 * parent.width

        SettingsRow {
            text: "Sound volume"
            CVarAwareSlider { cvarName: "s_volume" }
        }

        SettingsRow {
            text: "Music volume"
            CVarAwareSlider { cvarName: "s_musicvolume" }
        }

        SettingsGroupHeaderRow {
            visible: showGroupHeaders
            text: "Volume of in-game sounds"
        }

        SettingsRow {
            text: "Players sounds"
            CVarAwareSlider { cvarName: "cg_volume_players" }
        }

        SettingsRow {
            text: "Effects sounds"
            CVarAwareSlider { cvarName: "cg_volume_effects" }
        }

        SettingsRow {
            text: "Announcer sounds"
            CVarAwareSlider { cvarName: "cg_volume_announcer" }
        }

        SettingsRow {
            text: "Hit beep sounds"
            CVarAwareSlider { cvarName: "cg_volume_hitsound" }
        }

        SettingsGroupHeaderRow {
            visible: showGroupHeaders
            text: "Advanced effects"
        }

        SettingsRow {
            text: "Use environment effects"
            CVarAwareCheckBox {
                cvarName: "s_environment_effects"
                applyImmediately: false
            }
        }

        SettingsRow {
            text: "Use HRTF"
            CVarAwareCheckBox {
                cvarName: "s_hrtf"
                applyImmediately: false
            }
        }

        SettingsGroupHeaderRow {
            visible: showGroupHeaders
            text: "Miscellaneous settings"
        }

        SettingsRow {
            text: "Play sounds while in background"
            CVarAwareCheckBox { cvarName: "s_globalfocus" }
        }

        SettingsRow {
            text: "Chat message sound"
            CVarAwareCheckBox { cvarName: "cg_chatBeep" }
        }

        SettingsRow {
            text: "Heavy rocket explosions"
            CVarAwareCheckBox { cvarName: "cg_heavyRocketExplosions" }
        }

        SettingsRow {
            text: "Heavy grenade explosions"
            CVarAwareCheckBox { cvarName: "cg_heavyGrenadeExplosions" }
        }

        SettingsRow {
            text: "Heavy shockwave exploslions"
            CVarAwareCheckBox { cvarName: "cg_heavyShockwaveExplosions" }
        }
    }
}
