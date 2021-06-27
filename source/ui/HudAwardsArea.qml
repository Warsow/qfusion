import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root
    implicitHeight: listView.contentHeight + 144
    implicitWidth: rootItem.width

    ListView {
        id: listView
        anchors.centerIn: parent
        height: contentHeight
        width: parent.width
        model: hudDataModel.getAwardsModel()
        verticalLayoutDirection: ListView.BottomToTop
        spacing: 8

        add: Transition {
            ParallelAnimation {
                NumberAnimation {
                    property: "transformXScale"
                    from: 0.0; to: 1.0
                    easing.type: Easing.InElastic
                    easing.amplitude: 4.0
                    duration: 333
                }
                NumberAnimation {
                    property: "transformYScale"
                    from: 0.0; to: 1.0
                    easing.type: Easing.InElastic
                    easing.amplitude: 2.0
                    duration: 333
                }
            }
        }

        populate: Transition {
            ParallelAnimation {
                NumberAnimation {
                    property: "transformXScale"
                    from: 0.0; to: 1.0
                    easing.type: Easing.InElastic
                    easing.amplitude: 4.0
                    duration: 333
                }
                NumberAnimation {
                    property: "transformYScale"
                    from: 0.0; to: 1.0
                    easing.type: Easing.InElastic
                    easing.amplitude: 2.0
                    duration: 333
                }
            }
        }

        remove: Transition {
            ParallelAnimation {
                NumberAnimation {
                    property: "transformXScale"
                    from: 1.0; to: 0.0
                    easing.type: Easing.InCubic
                    duration: 67
                }
                NumberAnimation {
                    property: "transformYScale"
                    from: 1.0; to: 0.0
                    easing.type: Easing.InCubic
                    duration: 67
                }
            }
        }

        delegate: Label {
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
            text: model.message
            font.weight: Font.Black
            font.pointSize: 28 + 1 * index
            font.italic: true
            font.capitalization: Font.SmallCaps
            font.letterSpacing: 3
            font.wordSpacing: 3
        }
    }
}