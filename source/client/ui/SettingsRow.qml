import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    Layout.fillWidth: true
    property alias text: label.text
    property real spacing: UI.settingsRowSpacing
    property real explicitContentPartWidth: -1
    property bool rightToLeft: false

    implicitHeight: UI.settingsRowHeight

    // It's more maintainable to use a helper item (even it it's redundant) rather than playing with individual center offsets
    Item {
        id: separator
        width: spacing
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.horizontalCenter: parent.horizontalCenter
    }

    UILabel {
        id: label
        anchors.verticalCenter: parent.verticalCenter
        wrapMode: Text.WordWrap
        maximumLineCount: 99
        color: Material.foreground
        opacity: root.enabled ? 1.0 : 0.5
    }

    default property Item contentItem
    property real _contentItemLeftPadding
    property real _contentItemRightPadding

    // TODO: This is just for the initial setup, make switching in runtime correct
    states: [
        State {
            name: "anchoredLtr"
            AnchorChanges {
                target: label
                anchors.left: root.left
                anchors.right: separator.left
            }
            PropertyChanges {
                target: label
                horizontalAlignment: Qt.AlignRight
            }
            AnchorChanges {
                target: contentItem
                anchors.left: separator.right
                // Note that we don't stretch it
                anchors.right: undefined
                anchors.verticalCenter: root.verticalCenter
            }
            PropertyChanges {
                target: separator
                anchors.horizontalCenterOffset: explicitContentPartWidth >= 0 ?
                    +(0.5 * root.width - explicitContentPartWidth - 0.5 * spacing) : 0
            }
            PropertyChanges {
                target: contentItem
                anchors.leftMargin: -_contentItemLeftPadding
                anchors.rightMargin: 0
            }
        },
        State {
            name: "anchoredRtl"
            AnchorChanges {
                target: label
                anchors.left: separator.right
                anchors.right: root.right
            }
            PropertyChanges {
                target: label
                horizontalAlignment: Qt.AlignLeft
            }
            AnchorChanges {
                target: contentItem
                // Note that we don't stretch it
                anchors.left: undefined
                anchors.right: separator.left
                anchors.verticalCenter: root.verticalCenter
            }
            PropertyChanges {
                target: separator
                anchors.horizontalCenterOffset: explicitContentPartWidth >= 0 ?
                    -(0.5 * root.width - explicitContentPartWidth - 0.5 * spacing) : 0
            }
            PropertyChanges {
                target: contentItem
                anchors.leftMargin: 0
                anchors.rightMargin: -_contentItemRightPadding
            }
        }
    ]

    Component.onCompleted: {
        contentItem.parent = root
        console.assert(contentItem.hasOwnProperty("leftPadding") && contentItem.hasOwnProperty("rightPadding"))
        _contentItemLeftPadding = contentItem.leftPadding
        _contentItemRightPadding = contentItem.rightPadding
        state = rightToLeft ? "anchoredRtl" : "anchoredLtr"
    }
}