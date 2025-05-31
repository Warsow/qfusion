import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    property var currentValue
    property var chosenValue

    readonly property bool canProceed: typeof(chosenValue) !== "undefined"

    property var chosenNickname

    readonly property string displayedString: {
        if (typeof(chosenNickname) !== "undefined") {
            'Chosen <b>' + chosenNickname + '</b> for this action'
        } else {
            ""
        }
    }

    ListView {
        anchors.centerIn: parent
        width: parent.width
        height: Math.min(contentHeight, parent.height)
        model: UI.playersModel

        delegate: SelectableLabel {
            width: root.width
            height: 32
            text: nickname
            onClicked: {
                chosenValue = number
                chosenNickname = nickname
            }
        }
    }
}