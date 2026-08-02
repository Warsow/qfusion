import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import QtGraphicalEffects 1.12
import net.warsow 2.6

ListView {
    id: root

    verticalLayoutDirection: ListView.BottomToTop

    delegate: Loader {
        width: root.width
        sourceComponent: regularMessageText ? messageComponent : sectionComponent

        Component {
            id: sectionComponent
            Item {
                implicitWidth: root.width
                implicitHeight: Math.max(nameLabel.height, timestampLabel.height) + 20
                // We have to align everything to the right as this alingment makes the entire thing less biased to the left
                UILabel {
                    id: nameLabel
                    anchors.right: timestampLabel.left
                    anchors.baseline: timestampLabel.baseline
                    font.weight: Font.Black
                    font.letterSpacing: 1
                    textFormat: Text.StyledText
                    style: Text.Raised
                    text: model.sectionName
                }
                UILabel {
                    id: timestampLabel
                    anchors.right: parent.right
                    anchors.verticalCenter: parent.verticalCenter
                    anchors.verticalCenterOffset: 8
                    font.letterSpacing: 0
                    font.pointSize: UI.labelFontSize - 1.0
                    textFormat: Text.PlainText
                    text: " at " + model.sectionTimestamp
                    opacity: 0.7
                }
            }
        }

        Component {
            id: messageComponent
            UILabel {
                width: root.width
                height: implicitHeight + 8
                horizontalAlignment: Qt.AlignLeft
                verticalAlignment: Qt.AlignVCenter
                wrapMode: Text.WordWrap
                textFormat: Text.StyledText
                lineHeight: 1.1
                clip: true
                text: model.regularMessageText
            }
        }
    }
}