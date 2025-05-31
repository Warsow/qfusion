import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root
    implicitHeight: listView.contentHeight + 144 * actualScale
    // Just for anchoring other items
    implicitWidth: 0.5 * rootItem.width

    property var povDataModel
    property bool isMiniview
    property real miniviewScale: 1.0

    readonly property real defaultFontSize: 26
    readonly property real minFontSize: 12
    readonly property bool hasReachedDownscalingLimit: isMiniview ? (miniviewScale * defaultFontSize < minFontSize) : false
    readonly property real actualScale: hasReachedDownscalingLimit ? (minFontSize / defaultFontSize) : miniviewScale

    ListView {
        id: listView
        anchors.centerIn: parent
        height: contentHeight
        width: parent.width
        model: root.povDataModel.getAwardsModel()
        verticalLayoutDirection: ListView.BottomToTop
        spacing: 8 * actualScale

        add: Transition {
            NumberAnimation {
                property: "transformXScale"
                from: 0.0; to: 1.0
                easing.type: Easing.InOutElastic
                easing.amplitude: 10.0
                duration: 667
            }
            NumberAnimation {
                property: "transformYScale"
                from: 0.0; to: 1.0
                easing.type: Easing.InOutElastic
                easing.amplitude: 10.0
                duration: 667
            }
        }

        populate: Transition {
            NumberAnimation {
                property: "transformXScale"
                from: 0.0; to: 1.0
                easing.type: Easing.InOutElastic
                easing.amplitude: 10.0
                duration: 667
            }
            NumberAnimation {
                property: "transformYScale"
                from: 0.0; to: 1.0
                easing.type: Easing.InOutElastic
                easing.amplitude: 10.0
                duration: 667
            }
        }

        remove: Transition {
            NumberAnimation {
                property: "transformXScale"
                from: 1.0; to: 0.0
                easing.type: Easing.InCubic
                duration: 48
            }
            NumberAnimation {
                property: "transformYScale"
                from: 1.0; to: 0.0
                easing.type: Easing.InCubic
                duration: 48
            }
        }

        // A sentinel for interrupted other transitions
        displaced: Transition {
            NumberAnimation {
                property: "transformXScale"
                to: 1.0
            }
            NumberAnimation {
                property: "transformYScale"
                to: 1.0
            }
        }

        delegate: Label {
            id: listDelegate
            Component.onDestruction: Hud.ui.ensureObjectDestruction(listDelegate)
            // We must report individual items, so we correctly account for transitions
            Connections {
                target: Hud.ui
                enabled: !root.isMiniview
                onDisplayedHudItemsRetrievalRequested: Hud.ui.supplyDisplayedHudItemAndMargin(listDelegate, 4.0)
            }
            property real transformXScale
            property real transformYScale
            transform: Scale {
                origin.x: 0.5 * width
                origin.y: 0.5 * height
                xScale: transformXScale
                yScale: transformYScale
            }
            width: listView.width
            horizontalAlignment: Qt.AlignHCenter
            verticalAlignment: Qt.AlignVCenter
            font.weight: Font.Bold
            font.pointSize: defaultFontSize * actualScale
            font.capitalization: Font.SmallCaps
            font.letterSpacing: 2 * actualScale
            font.wordSpacing: 3 * actualScale
            style: hasReachedDownscalingLimit ? Text.Normal : Text.Raised
            textFormat: Text.StyledText
            text: model.message
        }
    }
}