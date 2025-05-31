import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root
    property int displayedAnchors
    property bool highlighted
    readonly property color color: "white"
    visible: displayedAnchors

    width: 10
    height: 10
    anchors.centerIn: parent

    readonly property int horizontalShiftSign:
        (displayedAnchors & HudLayoutModel.Left) ? -1 :
            ((displayedAnchors & HudLayoutModel.Right) ? +1 : 0)

    readonly property int verticalShiftSign:
        (displayedAnchors & HudLayoutModel.Top) ? -1 :
            ((displayedAnchors & HudLayoutModel.Bottom) ? +1 : 0)

    anchors.horizontalCenterOffset: 0.5 * horizontalShiftSign * parent.width
    anchors.verticalCenterOffset: 0.5 * verticalShiftSign * parent.height

    Rectangle {
        color: root.color
        width: 2
        height: verticalShiftSign ? 11 : 22
        anchors.centerIn: parent
        anchors.verticalCenterOffset: -0.5 * verticalShiftSign * parent.width
    }

    Rectangle {
        color: root.color
        width: horizontalShiftSign ? 11 : 22
        height: 2
        anchors.centerIn: parent
        anchors.horizontalCenterOffset: -0.5 * horizontalShiftSign * parent.height
    }
}