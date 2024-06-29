import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    implicitHeight: timeLabel.height + timeFlagsLabel.height

    property var timeMinutes
    property var timeSeconds
    property var timeFlags

    UILabel {
        id: timeLabel
        visible: height > 0
        height: (typeof(timeFlags) !== "undefined" && !timeFlags) ? implicitHeight : 0
        font.family: UI.ui.numbersFontFamily
        font.pointSize: 14
        font.letterSpacing: 8
        font.weight: Font.Black
        anchors.centerIn: parent
        horizontalAlignment: Qt.AlignHCenter
        text: (timeMinutes ? (timeMinutes < 10 ? "0" + timeMinutes : timeMinutes) : "00") +
              ":" +
              (timeSeconds ? (timeSeconds < 10 ? "0" + timeSeconds : timeSeconds) : "00")
    }

    UILabel {
        id: timeFlagsLabel
        visible: height > 0
        height: text.length > 0 ? implicitHeight : 0
        text: formatTimeFlags()
        anchors.centerIn: parent
        font.family: UI.ui.headingFontFamily
        font.pointSize: 13
        font.letterSpacing: 1.75
        font.weight: Font.Black
        horizontalAlignment: Qt.AlignHCenter

        Connections {
            target: root
            onTimeFlagsChanged: {
                timeFlagsLabel.text = timeFlagsLabel.formatTimeFlags()
            }
        }

        function formatTimeFlags() {
            if (!timeFlags) {
                return ""
            }

            if (timeFlags & ServerListModel.Warmup) {
                if (!!playersTeamList || !!alphaTeamList || !!betaTeamList) {
                    return "WARMUP"
                }
                return ""
            }

            if (timeFlags & ServerListModel.Countdown) {
                return "COUNTDOWN"
            }

            if (timeFlags & ServerListModel.Finished) {
                return "FINISHED"
            }

            let s = ""
            if (timeFlags & ServerListModel.SuddenDeath) {
                s += "SUDDEN DEATH, "
            } else if (timeFlags & ServerListModel.Overtime) {
                s += "OVERTIME, "
            }

            // Actually never set for warmups. let's not complicate
            if (timeFlags & ServerListModel.Timeout) {
                s += "TIMEOUT, "
            }

            if (s.length > 2) {
                s = s.substring(0, s.length - 2)
            }
            return s
        }
    }
}