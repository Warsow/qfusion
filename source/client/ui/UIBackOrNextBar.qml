import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: control
    width: UI.acceptRejectRowWidth
    height: UI.acceptRejectRowHeight

    signal backButtonClicked()
    signal nextButtonClicked()
    property alias backButtonText: backButton.text
    property alias backButtonVisible: backButton.visible
    property alias nextButtonText: nextButton.text
    property alias nextButtonVisible: nextButton.visible

    SlantedLeftSecondaryButton {
        id: backButton
        anchors.left: parent.left
        anchors.verticalCenter: parent.verticalCenter
        text: "back"
        onClicked: control.backButtonClicked()
    }

    SlantedRightPrimaryButton {
        id: nextButton
        anchors.right: parent.right
        anchors.verticalCenter: parent.verticalCenter
        highlighted: true
        text: "next"
        onClicked: control.nextButtonClicked()
    }
}