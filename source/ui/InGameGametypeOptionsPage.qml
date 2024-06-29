import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root

    readonly property real optionWidth: 108
    readonly property real optionExtraWidthOnMouseOver: 12
    readonly property real optionExtraHeightOnMouseOver: 4
    readonly property real optionBodySlantDegrees: 15
    readonly property real optionSpacing: 18
    readonly property real maxOptionsPerRow: 3

    readonly property var booleanOptionTexts: ["Off", "On"]
    readonly property var booleanOptionVals: [0, 1]
    readonly property var booleanOptionPredicates: [(x) => x === 0, (x) => x !== 0]
    readonly property var booleanOptionIcons: ["image://wsw/gfx/hud/icons/vsay/no", "image://wsw/gfx/hud/icons/vsay/yes"]

    ListView {
        id: list
        anchors.centerIn: parent
        width: parent.width
        height: parent.height > contentHeight ? contentHeight : parent.height
        boundsBehavior: parent.height > contentHeight ? Flickable.StopAtBounds : Flickable.OvershootBounds
        spacing: parent.height > contentHeight ? 48 : 36
        model: UI.gametypeOptionsModel

        delegate: Item {
            id: option
            implicitWidth: loader.width
            implicitHeight: titleLabel.implicitHeight + loader.height + loader.anchors.topMargin

            readonly property int optionRow: index
            readonly property int optionSelectionLimit: selectionLimit
            readonly property int optionNumItems: numItems
            readonly property var optionCurrent: current

            UILabel {
                id: titleLabel
                anchors.top: parent.top
                width: root.width
                horizontalAlignment: Qt.AlignHCenter
                text: title
                font.weight: Font.Medium
                font.pointSize: 15
                font.capitalization: Font.SmallCaps
                font.letterSpacing: 1
            }

            Loader {
                id: loader
                anchors.top: titleLabel.bottom
                anchors.topMargin: 24
                anchors.horizontalCenter: parent.horizontalCenter

                width: root.width
                height: item ? item.implicitHeight : 0

                sourceComponent: (kind === GametypeOptionsModel.Boolean) ? booleanComponent : selectorComponent

                Component {
                    id: booleanComponent
                    RowLayout {
                        width: root.width
                        spacing: optionSpacing

                        Item { Layout.fillWidth: true }

                        Repeater {
                            model: 2
                            delegate: SlantedButton {
                                text: booleanOptionTexts[index]
                                leftBodyPartSlantDegrees: optionBodySlantDegrees
                                rightBodyPartSlantDegrees: optionBodySlantDegrees
                                textSlantDegrees: 0.0
                                extraWidthOnMouseOver: optionExtraWidthOnMouseOver
                                extraHeightOnMouseOver: optionExtraHeightOnMouseOver
                                Layout.preferredWidth: optionWidth
                                checked: booleanOptionPredicates[index](option.optionCurrent)
                                iconPath: booleanOptionIcons[index]
                                onClicked: UI.gametypeOptionsModel.select(option.optionRow, booleanOptionVals[index])
                            }
                        }

                        Item { Layout.fillWidth: true }
                    }
                }

                Component {
                    id: selectorComponent
                    ColumnLayout {
                        id: selectorItem

                        // This is a local queue for cycling selected indices during the selector lifetime.
                        // To make it work, the native code tries to limit the scope of dispatched updates to optionCurrent when possible.
                        property var selectedIndices: []

                        Component.onCompleted: selectorItem.selectedIndices = [...option.optionCurrent]

                        spacing: 24
                        width: root.width
                        Repeater {
                            model: Math.max(option.optionNumItems / maxOptionsPerRow, 1)
                            RowLayout {
                                readonly property int rowIndex: index

                                width: root.width
                                spacing: optionSpacing

                                Item { Layout.fillWidth: true }

                                Repeater {
                                    model: rowIndex != Math.floor(option.optionNumItems / maxOptionsPerRow) ?
                                        maxOptionsPerRow : option.optionNumItems % maxOptionsPerRow
                                    delegate: SlantedButton {
                                        readonly property int flatIndex: rowIndex * maxOptionsPerRow + index
                                        Layout.preferredWidth: optionWidth
                                        leftBodyPartSlantDegrees: optionBodySlantDegrees
                                        rightBodyPartSlantDegrees: optionBodySlantDegrees
                                        textSlantDegrees: 0.0
                                        extraWidthOnMouseOver: optionExtraWidthOnMouseOver
                                        extraHeightOnMouseOver: optionExtraHeightOnMouseOver
                                        checked: option.optionCurrent.includes(flatIndex)
                                        iconPath: UI.gametypeOptionsModel.getSelectorItemIcon(option.optionRow, flatIndex)
                                        text: UI.gametypeOptionsModel.getSelectorItemTitle(option.optionRow, flatIndex)
                                        onClicked: {
                                            // This leads to model updates, which are likely to affect option.optionCurrent
                                            if (!checked) {
                                                console.assert(!selectorItem.selectedIndices.includes(flatIndex))
                                                selectorItem.selectedIndices.shift()
                                                selectorItem.selectedIndices.push(flatIndex)
                                                console.assert(selectorItem.selectedIndices.length === optionSelectionLimit)
                                                UI.ui.playSwitchSound()
                                                UI.gametypeOptionsModel.select(option.optionRow, selectorItem.selectedIndices)
                                            }
                                        }
                                    }
                                }

                                Item { Layout.fillWidth: true }
                            }
                        }
                    }
                }
            }
        }
    }
}