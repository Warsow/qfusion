import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

FocusScope {
    id: introScreen
    ColumnLayout {
        id: columnLayout
        anchors.centerIn: parent
        width: 720
        Item {
            Layout.preferredWidth: 0
            Layout.preferredHeight: title.height
            Layout.alignment: Qt.AlignHCenter
            Layout.bottomMargin: 24

            UILabel {
                id: title
                anchors.centerIn: parent
                text: "Warsow is Art of Respect and Sportsmanship Over the Web"
                font.pointSize: 20
                font.weight: Font.Medium
                font.capitalization: Font.SmallCaps
            }
        }

        RowLayout {
            Layout.preferredWidth: parent.width / 2
            Layout.maximumWidth: parent.width / 2
            Layout.alignment: Qt.AlignHCenter
            spacing: 8

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: UI.boldLineHeight
                radius: 1
                color: Material.foreground
            }

            UILabel {
                text: "\u00A7"
                Layout.alignment: Qt.AlignHCenter
                font.family: UI.ui.numbersFontFamily
                font.pointSize: 24
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: UI.boldLineHeight
                radius: 1
                color: Material.foreground
            }
        }

        UILabel {
            maximumLineCount: 10
            Layout.topMargin: 24
            Layout.preferredWidth: columnLayout.width
            wrapMode: Text.WordWrap
            horizontalAlignment: Text.AlignJustify
            text:
                "Since 2005, Warsow is considered as one of the most skill-demanding games in the fast-paced arena shooter scene. " +
                "If you're looking for some challenge or old-skool and hardcore gameplay, you came to the right place!"
        }
        UILabel {
            maximumLineCount: 10
            Layout.topMargin: 16
            Layout.bottomMargin: 16
            Layout.preferredWidth: columnLayout.width
            text: "Here's a few tips that will improve your gaming experience in Warsow:"
        }
        UILabel {
            text: "\u2022 GL, HF & GG - these are the fundamentals of esports. It never hurts to say them;"
        }
        UILabel {
            text: "\u2022 Each loss is an opportunity to improve your skills and game knowledge;"
        }
        UILabel {
            text: "\u2022 Each win is an opportunity to share your game knowledge with the others!"
        }
        UILabel {
            Layout.topMargin: 16
            maximumLineCount: 10
            Layout.preferredWidth: columnLayout.width
            text: "Thanks for your attention and once again, welcome to Warsow!"
        }
        UILabel {
            Layout.topMargin: 16
            maximumLineCount: 10
            Layout.preferredWidth: columnLayout.width
            horizontalAlignment: Qt.AlignRight
            text: "Fabrice Demurger, Warsow founder"
            opacity: 0.7
        }

        RowLayout {
            Layout.alignment: Qt.AlignHCenter
            Layout.topMargin: 48
            spacing: UI.minAcceptRejectSpacing
            SlantedButton {
                id: disagreeButton
                Layout.preferredWidth: UI.neutralCentralButtonWidth
                text: "Disagree"
                highlightOnActiveFocus: true
                KeyNavigation.right: agreeButton
                leftBodyPartSlantDegrees: -1.0 * UI.buttonBodySlantDegrees
                rightBodyPartSlantDegrees: -0.3 * UI.buttonBodySlantDegrees
                textSlantDegrees: -0.3 * UI.buttonTextSlantDegrees
                labelHorizontalCenterOffset: 0
                onClicked: introScreen.handleDisagreeAction()
                Keys.onEnterPressed: introScreen.handleDisagreeAction()
            }
            SlantedButton {
                id: agreeButton
                Layout.preferredWidth: UI.neutralCentralButtonWidth
                text: "Agree"
                focus: true
                highlightOnActiveFocus: true
                KeyNavigation.left: disagreeButton
                leftBodyPartSlantDegrees: +0.3 * UI.buttonBodySlantDegrees
                rightBodyPartSlantDegrees: +1.0 * UI.buttonBodySlantDegrees
                textSlantDegrees: +0.3 * UI.buttonTextSlantDegrees
                labelHorizontalCenterOffset: 0
                // TODO: Get rid of this, set up focus scopes properly
                Component.onCompleted: forceActiveFocus()
                onClicked: introScreen.handleAgreeAction()
                Keys.onEnterPressed: introScreen.handleAgreeAction()
            }
        }
    }
    function handleAgreeAction() {
        UI.ui.playForwardSound()
        UI.ui.finishIntro()
    }
    function handleDisagreeAction() {
        UI.ui.playBackSound()
        UI.ui.quit()
    }
}