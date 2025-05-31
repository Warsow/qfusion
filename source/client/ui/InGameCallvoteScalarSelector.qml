import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    property var currentValue
    property var chosenValue

    property Component delegate
    property var valueFormatter: v => '' + v

    readonly property bool canProceed: typeof(chosenValue) !== "undefined" &&
        ("" + chosenValue).length > 0 && chosenValue != currentValue

    readonly property string displayedString: {
        if (canProceed) {
            'Chosen <b>' + valueFormatter(chosenValue) + '</b> over <b>' + valueFormatter(currentValue) + '</b>'
        } else {
            '<b>' + valueFormatter(currentValue) + '</b> is the current value'
        }
    }

    Loader {
        anchors.centerIn: parent
        sourceComponent: delegate
    }
}