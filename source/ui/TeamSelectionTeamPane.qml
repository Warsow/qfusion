import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root

    implicitHeight: stripe.height + listView.height + listView.anchors.topMargin + listView.anchors.bottomMargin

    property color color
    property var model
    property int alignment

    readonly property color backgroundColor: Qt.darker(root.color, 1.75)

    Rectangle {
        id: stripe
        anchors.left: parent.left
        anchors.right: parent.right
        height: 12
        color: Qt.darker(root.color, 1.25)
        opacity: 0.6
    }

    ListView {
        id: listView
        model: root.model
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: stripe.bottom
        anchors.leftMargin: 8
        anchors.rightMargin: 8
        anchors.topMargin: 8
        anchors.bottomMargin: 8
        height: contentHeight
        interactive: false
        delegate: Item {
            height: 32
            width: listView.width
            Rectangle {
                anchors.fill: parent
                color: (index + (root.alignment === Qt.AlignLeft ? 0 : 1)) % 2 ? Qt.darker(backgroundColor, 1.1) : Qt.lighter(backgroundColor, 1.2)
                opacity: 0.3
            }
            Label {
                anchors.fill: parent
                horizontalAlignment: root.alignment
                verticalAlignment: Qt.AlignVCenter
                leftPadding: 5
                rightPadding: 5
                text: value
                maximumLineCount: 1
                elide: root.alignment === Qt.AlignLeft ? Text.ElideRight : Text.ElideLeft
                font.pointSize: UI.scoreboardFontSize
                font.letterSpacing: UI.scoreboardLetterSpacing
                font.weight: Font.Bold
                style: Text.Raised
            }
        }
    }

    Rectangle {
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: stripe.bottom
        height: listView.anchors.topMargin
        color: backgroundColor
        opacity: 0.3
    }

    Rectangle {
        anchors.top: listView.top
        anchors.bottom: listView.bottom
        anchors.left: parent.left
        width: listView.anchors.leftMargin
        color: backgroundColor
        opacity: 0.3
    }

    Rectangle {
        anchors.top: listView.top
        anchors.bottom: listView.bottom
        anchors.right: parent.right
        width: listView.anchors.rightMargin
        color: backgroundColor
        opacity: 0.3
    }

    Rectangle {
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: listView.bottom
        anchors.bottom: parent.bottom
        color: backgroundColor
        opacity: 0.3
    }

    UILabel {
        anchors.top: stripe.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.topMargin: listView.anchors.topMargin
        anchors.leftMargin: listView.anchors.leftMargin
        anchors.rightMargin: listView.anchors.rightMargin
        leftPadding: 5
        rightPadding: 5
        height: 32
        horizontalAlignment: root.alignment
        verticalAlignment: Qt.AlignVCenter
        visible: listView.count === 0
        text: "(no players)"
        font.weight: Font.Bold
        font.letterSpacing: 1.5
        style: Text.Raised
    }
}