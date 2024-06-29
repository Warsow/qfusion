import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    readonly property int weaponsPerRow: 3
    readonly property var accuracyModel: UI.scoreboard.accuracyModel
    readonly property int numWeapons: accuracyModel.length

    visible: numWeapons
    implicitHeight: visible ?
        (label.anchors.topMargin + label.implicitHeight + label.anchors.topMargin +
        column.anchors.topMargin + column.implicitHeight + column.anchors.bottomMargin + 8) : 0

    Rectangle {
        anchors.fill: parent
        color: "black"
        opacity: 0.5
    }

    UILabel {
        id: label
        anchors.top: parent.top
        anchors.topMargin: 8
        anchors.horizontalCenter: parent.horizontalCenter
        font.family: UI.ui.headingFontFamily
        font.capitalization: Font.AllUppercase
        font.letterSpacing: 1.0
        font.weight: Font.Bold
        text: "Accuracy"
        style: Text.Raised
    }

    ColumnLayout {
        id: column
        width: parent.width - 8
        anchors.top: label.bottom
        anchors.topMargin: 16 + 2
        anchors.horizontalCenter: parent.horizontalCenter
        spacing: 8

        Repeater {
            id: rowsRepeater
            model: numWeapons ? Math.floor(numWeapons / weaponsPerRow) + 1 : 0
            delegate: Row {
                Layout.alignment: Qt.AlignHCenter
                readonly property int rowIndex: index
                spacing: 16
                Repeater {
                    model: (rowIndex + 1 != rowsRepeater.count) ? weaponsPerRow : (numWeapons % weaponsPerRow)
                    delegate: Item {
                        width: nameLabel.implicitWidth + 8 + valuesLabel.implicitWidth
                        height: Math.max(nameLabel.implicitHeight, valuesLabel.implicitHeight)
                        readonly property var entry: accuracyModel[rowIndex * weaponsPerRow + index]
                        Label {
                            id: nameLabel
                            anchors.left: parent.left
                            anchors.baseline: parent.bottom
                            font.family: UI.ui.regularFontFamily
                            font.weight: Font.Bold
                            font.capitalization: Font.AllUppercase
                            font.pointSize: UI.scoreboardFontSize
                            font.letterSpacing: UI.scoreboardLetterSpacing
                            color: UI.hudCommonDataModel.getWeaponColor(entry["weapon"])
                            text: UI.hudCommonDataModel.getWeaponShortName(entry["weapon"])
                        }
                        Label {
                            id: valuesLabel
                            anchors.right: parent.right
                            anchors.baseline: parent.bottom
                            font.family: UI.ui.numbersFontFamily
                            font.weight: Font.Bold
                            font.capitalization: Font.AllUppercase
                            font.pointSize: UI.scoreboardFontSize
                            font.letterSpacing: UI.scoreboardLetterSpacing
                            text: entry["strong"] + "%/" + entry["weak"] + "%"
                        }
                    }
                }
            }
        }
    }
}

