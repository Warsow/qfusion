import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root
    implicitWidth: 12
    height: 12
    clip: false

    property alias containsMouse: mouseArea.containsMouse

    property int layoutIndex
    property color haloColor

    property Item contentItem

    Component.onCompleted: contentItem.parent = contentItemContainer

    signal mouseEnter(int index)
    signal mouseLeave(int index)
    signal clicked(int index)

    NumberAnimation {
        id: shiftAnim
        target: contentItemTranslation
        property: "x"
        duration: 150
        easing.type: Easing.OutCubic
    }

    NumberAnimation {
        id: haloAnim
        target: halo
        property: "opacity"
        duration: 250
    }

    function startShift(rightToLeft) {
        shiftAnim.stop()
        shiftAnim.from = contentItemTranslation.x
        shiftAnim.to = rightToLeft ? -9 : 9
        shiftAnim.start()
    }

    function revertShift() {
        shiftAnim.stop()
        shiftAnim.from = contentItemTranslation.x
        shiftAnim.to = 0
        shiftAnim.start()
    }

    function startExtraHoverAnims() {}
    function stopExtraHoverAnims() {}

    MouseArea {
        id: mouseArea
        anchors.centerIn: parent
        width: 12; height: 12
        hoverEnabled: true
        onContainsMouseChanged: {
            if (containsMouse) {
                UI.ui.playHoverSound()

                root.mouseEnter(root.layoutIndex)

                haloAnim.stop()
                haloAnim.from = halo.opacity
                haloAnim.to = 0.33
                haloAnim.start()

                startExtraHoverAnims()
            } else {
                root.mouseLeave(root.layoutIndex)

                haloAnim.stop()
                haloAnim.from = halo.opacity
                haloAnim.to = 0.0
                haloAnim.start()

                stopExtraHoverAnims()
            }
        }
        onClicked: {
            UI.ui.playSwitchSound()
            root.clicked(root.layoutIndex)
        }
    }

    Rectangle {
        id: halo
        anchors.centerIn: parent
        color: Qt.lighter(root.haloColor)
        opacity: 0.0
        width: 32
        height: 32
        radius: 16
    }

    Item {
        id: contentItemContainer
        clip: false
        anchors.centerIn: parent
        width: 12
        height: 12
        transform: Translate {
            id: contentItemTranslation
        }
    }
}