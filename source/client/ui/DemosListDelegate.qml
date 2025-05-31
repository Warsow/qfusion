import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

MouseArea {
    id: root
    height: compact ? UI.demoCompactRowHeight : UI.demoWideRowHeight
    hoverEnabled: true

    property bool compact
    property bool selected

    property string demoName
    property string serverName
    property string mapName
    property string gametype
    property string timestamp
    // TODO: An eager retrieval of tags is bad, check whether using a lazy binding (?) to a model property is possible
    property string tags

    property real demoColumnWidth
    property real serverColumnWidth
    property real mapColumnWidth
    property real gametypeColumnWidth
    property real timestampColumnWidth

    property real rowSpacing
    property real labelMargins: rowSpacing / 3

    readonly property color textColor: containsMouse || selected ? Material.accent : Material.foreground

    transitions: Transition {
        AnchorAnimation { duration: 67 }
    }

    Behavior on height {
        NumberAnimation { duration: 67 }
    }

    states: [
        State {
            name: "linear"
            when: !compact
            AnchorChanges {
                target: demoNameLabel
                anchors.left: root.left
                anchors.right: undefined
                anchors.top: undefined
                anchors.bottom: root.bottom
            }
            AnchorChanges {
                target: serverNameLabel
                anchors.left: demoNameLabel.right
                anchors.right: undefined
                anchors.top: undefined
                anchors.bottom: root.bottom
            }
            AnchorChanges {
                target: mapNameLabel
                anchors.left: serverNameLabel.right
                anchors.right: undefined
                anchors.top: undefined
                anchors.bottom: root.bottom
            }
            AnchorChanges {
                target: gametypeLabel
                anchors.left: mapNameLabel.right
                anchors.right: undefined
                anchors.top: undefined
                anchors.bottom: root.bottom
            }
            AnchorChanges {
                target: timestampLabel
                anchors.left: undefined
                anchors.right: root.right
                anchors.top: undefined
                anchors.bottom: root.bottom
            }
            PropertyChanges {
                target: demoNameLabel
                width: demoColumnWidth
                font.weight: Font.Medium
                font.capitalization: Font.Normal
            }
            PropertyChanges {
                target: serverNameLabel
                horizontalAlignment: Qt.AlignHCenter
                font.weight: Font.Medium
                elide: Text.ElideMiddle
            }
            PropertyChanges {
                target: mapNameLabel
                width: mapColumnWidth
                font.weight: Font.Medium
            }
            PropertyChanges {
                target: gametypeLabel
                width: gametypeColumnWidth
                font.weight: Font.Medium
            }
            PropertyChanges {
                target: timestampLabel
                font.weight: Font.Medium
            }
            PropertyChanges {
                target: tagsLabel
                visible: false
            }
        },
        State {
            name: "compact"
            when: compact
            AnchorChanges {
                target: demoNameLabel
                anchors.left: root.left
                anchors.right: undefined
                anchors.top: root.top
                anchors.bottom: undefined
            }
            AnchorChanges {
                target: serverNameLabel
                anchors.left: root.left
                anchors.right: undefined
                anchors.top: demoNameLabel.bottom
                anchors.bottom: undefined
            }
            AnchorChanges {
                target: mapNameLabel
                anchors.left: undefined
                anchors.right: gametypeLabel.left
                anchors.top: timestampLabel.bottom
                anchors.bottom: undefined
            }
            AnchorChanges {
                target: gametypeLabel
                anchors.left: undefined
                anchors.right: root.right
                anchors.top: timestampLabel.bottom
                anchors.bottom: undefined
            }
            AnchorChanges {
                target: timestampLabel
                anchors.left: undefined
                anchors.right: root.right
                anchors.top: root.top
                anchors.bottom: undefined
            }
            PropertyChanges {
                target: demoNameLabel
                width: root.width - timestampLabel.implicitWidth - 24
                font.capitalization: Font.AllUppercase
                font.weight: Font.Bold
            }
            PropertyChanges {
                target: serverNameLabel
                horizontalAlignment: Qt.AlignLeft
                font.capitalization: Font.MixedCase
                font.weight: Font.Normal
                elide: Text.ElideRight
            }
            PropertyChanges {
                target: mapNameLabel
                font.capitalization: Font.MixedCase
                width: mapNameLabel.implicitWidth
                font.weight: Font.Normal
            }
            PropertyChanges {
                target: gametypeLabel
                font.capitalization: Font.MixedCase
                width: gametypeLabel.implicitWidth
                font.weight: Font.Normal
            }
            PropertyChanges {
                target: timestampLabel
                font.weight: Font.Bold
            }
            PropertyChanges {
                target: tagsLabel
                visible: true
            }
        }
    ]

    state: "linear"

    UILabel {
        id: demoNameLabel
        anchors.margins: labelMargins
        horizontalAlignment: Qt.AlignLeft
        font.weight: Font.Medium
        elide: Text.ElideRight
        text: demoName
        color: textColor
    }
    UILabel {
        id: serverNameLabel
        width: serverColumnWidth
        anchors.margins: labelMargins
        horizontalAlignment: Qt.AlignHCenter
        font.weight: Font.Medium
        text: serverName
        color: textColor
    }
    UILabel {
        id: mapNameLabel
        anchors.margins: labelMargins
        horizontalAlignment: Qt.AlignHCenter
        font.weight: Font.Medium
        text: mapName
        color: textColor
    }
    UILabel {
        id: gametypeLabel
        anchors.margins: labelMargins
        horizontalAlignment: Qt.AlignHCenter
        font.weight: Font.Medium
        text: gametype
        color: textColor
    }
    UILabel {
        id: timestampLabel
        width: timestampColumnWidth
        anchors.margins: labelMargins
        horizontalAlignment: Qt.AlignRight
        font.weight: Font.Medium
        text: timestamp
        color: textColor
    }
    UILabel {
        id: tagsLabel
        anchors.left: parent.left
        anchors.top: serverNameLabel.bottom
        anchors.margins: labelMargins
        width: parent.width
        maximumLineCount: 1
        elide: Text.ElideMiddle
        text: tags
        color: textColor
        opacity: 0.7
    }
}