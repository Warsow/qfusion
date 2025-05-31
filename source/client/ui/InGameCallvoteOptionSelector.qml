import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root

    property var currentValue
    property var chosenValue
    property var model

    readonly property bool canProceed: typeof(chosenValue) !== "undefined" && chosenValue != currentValue

    readonly property string displayedString: {
        if (canProceed) {
            'Chosen <b>' + chosenValue + '</b> over <b>' + currentValue + '</b>'
        } else {
            '<b>' + currentValue + '</b> is the current value'
        }
    }

    ListView {
        anchors.centerIn: parent
        width: parent.width
        height: Math.min(contentHeight, parent.height)

        boundsBehavior: Flickable.StopAtBounds
        model: root.model
        spacing: 8

        delegate: SelectableLabel {
            width: parent.width
            height: 28
            horizontalAlignment: Qt.AlignHCenter
            text: modelData["name"]
            onClicked: chosenValue = modelData["value"]
        }
    }
}