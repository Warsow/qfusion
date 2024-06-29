import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Flickable {
    id: root
    flickableDirection: Flickable.VerticalFlick

    ColumnLayout {
        anchors.centerIn: parent
        width: 0.67 * parent.width

        SettingsGroupHeaderRow { text: "Screen" }

        SettingsRow {
            text: "Display resolution"
            CVarAwareVideoModesComboBox {}
        }

        SettingsRow {
            text: "FPS Limit"
            CVarAwareComboBox {
                knownHeadingsAndValues: [["(none)", 1000, 500, 333, 250, 125, 60], [0, 1000, 500, 333, 250, 125, 60]]
                cvarName: "cl_maxfps"
            }
        }

        SettingsRow {
            text: "Fullscreen"
            CVarAwareCheckBox {
                cvarName: "vid_fullscreen"
                applyImmediately: false
            }
        }

        SettingsRow {
            text: "Vertical synchronization"
            CVarAwareCheckBox {
                cvarName: "r_swapinterval"
                applyImmediately: false
            }
        }

        SettingsGroupHeaderRow { text: "Effects" }

        SettingsRow {
            text: "Dynamic lighting"
            CVarAwareComboBox {
                knownHeadingsAndValues: [["Combined", "Shader", "Fake", "Off"], [-1, 1, 2, 0]]
                cvarName: "r_dynamiclight"
            }
        }

        SettingsRow {
            text: "Particles"
            CVarAwareCheckBox { cvarName: "cg_particles" }
        }

        SettingsRow {
            text: "Decoration details"
            CVarAwareCheckBox { cvarName: "r_detailtextures" }
        }

        SettingsRow {
            text: "Explosions/impact wave"
            CVarAwareCheckBox { cvarName: "cg_explosionWave" }
        }

        SettingsRow {
            text: "Explosions smoke"
            CVarAwareCheckBox { cvarName: "cg_explosionSmoke" }
        }

        SettingsGroupHeaderRow { text: "Esport features" }

        SettingsRow {
            text: "Use solid colors for world"
            CVarAwareCheckBox {
                id: drawFlatCheckBox
                cvarName: "r_drawflat"
            }
        }

        SettingsRow {
            enabled: drawFlatCheckBox.checked
            text: "Wall color"
            CVarAwareColorPicker { cvarName: "r_wallcolor" }
        }

        SettingsRow {
            enabled: drawFlatCheckBox.checked
            text: "Floor color"
            CVarAwareColorPicker { cvarName: "r_floorcolor" }
        }
    }
}
