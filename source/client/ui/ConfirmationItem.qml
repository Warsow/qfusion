import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

FocusScope {
    id: root

    property alias titleText: titleLabel.text

    property Component contentComponent

    signal buttonFocus
    signal buttonClicked(int buttonIndex)
    signal buttonActiveFocusChanged(int buttonIndex, bool buttonActiveFocus)

    property int numButtons
    // They are separate instead of being field of some model aggregate to avoid a full reset during model updates
    property var buttonTexts: []
    property var buttonEnabledStatuses: []
    property var buttonFocusStatuses: []

    property real contentTopMargin: 16
    property real buttonsTopMargin: 36
    property real columnWidth: UI.desiredPopupContentWidth

    property bool contentToButtonsKeyNavigationTargetIndex

    property bool _isReactingToFocusChanges

    onActiveFocusChanged: applyActiveFocus()

    Component.onCompleted: {
        opacityAnim.start()
        scaleAnim.start()
        applyActiveFocus()
        setupKeyNavigation()
    }

    onButtonFocusStatusesChanged: {
        if (!root._isReactingToFocusChanges) {
            if (buttonsRepeater.numInstantiatedItems === root.numButtons) {
                for (let i = 0; i < root.buttonFocusStatuses.length; ++i) {
                    if (root.buttonFocusStatuses[i]) {
                        buttonsRepeater.itemAt(i).forceActiveFocus()
                    }
                }
            }
        }
    }

    function applyActiveFocus() {
        if (root.activeFocus) {
            if (buttonsRepeater.numInstantiatedItems === root.numButtons) {
                if (contentLoader.item && contentLoader.item.hasOwnProperty("focusable")) {
                    contentLoader.item.forceActiveFocus()
                } else {
                    for (let i = 0; i < root.buttonFocusStatuses.length; ++i) {
                        if (root.buttonFocusStatuses[i]) {
                            buttonsRepeater.itemAt(i).forceActiveFocus()
                        }
                    }
                }
            }
        }
    }

    onContentToButtonsKeyNavigationTargetIndexChanged: setupKeyNavigation()

    function setupKeyNavigation() {
        if (root.numButtons) {
            if (buttonsRepeater.numInstantiatedItems === root.numButtons) {
                let focusableContentItem = null
                if (contentLoader.item) {
                    if (contentLoader.item.hasOwnProperty("focusable") && contentLoader.item["focusable"]) {
                        focusableContentItem = contentLoader.item
                    }
                }
                if (focusableContentItem) {
                    focusableContentItem.KeyNavigation.down = buttonsRepeater.itemAt(root.contentToButtonsKeyNavigationTargetIndex)
                }
                for (let i = 0; i < root.numButtons; ++i) {
                    const item = buttonsRepeater.itemAt(i)
                    if (i) {
                        item.KeyNavigation.left = buttonsRepeater.itemAt(i - 1);
                    }
                    if (i + 1 < root.numButtons) {
                        item.KeyNavigation.right = buttonsRepeater.itemAt(i + 1);
                    }
                    item.KeyNavigation.up = focusableContentItem
                }
            }
        }
    }

    Connections {
        target: contentLoader.item
        enabled: contentLoader.item && contentLoader.item.hasOwnProperty("focusable")
        ignoreUnknownSignals: true
        onFocusableChanged: root.setupKeyNavigation()
    }

    NumberAnimation {
        id: opacityAnim
        target: primaryColumn
        property: "opacity"
        duration: 100
        to: 1.0
    }
    NumberAnimation {
        id: scaleAnim
        target: primaryColumn
        property: "scale"
        duration: 100
        to: 1.0
    }

    ColumnLayout {
        id: primaryColumn
        opacity: 0.0
        scale: 0.0
        anchors.centerIn: parent
        width: columnWidth
        focus: true

        UILabel {
            id: titleLabel
            Layout.fillWidth: true
            horizontalAlignment: Text.AlignHCenter
            font.pointSize: 18
            font.letterSpacing: 1.25
            font.capitalization: Font.SmallCaps
            font.weight: Font.Medium
        }

        Rectangle {
            visible: !!contentLoader.item
            color: Material.foreground
            Layout.topMargin: visible ? 16 : 0
            Layout.alignment: Qt.AlignHCenter
            Layout.preferredWidth: 0
            Layout.preferredHeight: visible ? UI.boldLineHeight : 0
            Component.onCompleted: {
                if (visible) {
                    Layout.preferredWidth = 0.67 * primaryColumn.width
                }
            }
            Behavior on Layout.preferredWidth { NumberAnimation { duration: 300 } }
        }

        Loader {
            id: contentLoader
            Layout.alignment: Text.AlignHCenter
            Layout.topMargin: contentTopMargin
            Layout.preferredWidth: item ? item.implicitWidth : 0
            Layout.preferredHeight: item ? item.implicitHeight : 0
            sourceComponent: root.contentComponent
            onItemChanged: root.setupKeyNavigation()
        }

        RowLayout {
            Layout.alignment: Qt.AlignHCenter
            Layout.topMargin: buttonsTopMargin
            spacing: UI.minAcceptRejectSpacing

            Repeater {
                id: buttonsRepeater
                property int numInstantiatedItems
                model: root.numButtons
                delegate: SlantedButton {
                    Layout.preferredWidth:
                        (root.numButtons === 1) ? UI.neutralCentralButtonWidth : UI.acceptOrRejectButtonWidth
                    text: root.buttonTexts[index]
                    enabled: root.buttonEnabledStatuses[index]
                    highlighted: root.buttonFocusStatuses[index]
                    // Don't try to manage it on its own
                    highlightOnActiveFocus: false
                    leftBodyPartSlantDegrees:
                        (root.numButtons === 1) ? -0.5 * UI.buttonBodySlantDegrees :
                            (((index === 0) ? -1.0 : +0.3) * UI.buttonBodySlantDegrees)
                    rightBodyPartSlantDegrees:
                        (root.numButtons === 1) ? +0.5 * UI.buttonBodySlantDegrees :
                            (((index === 0) ? -0.3 : +1.0) * UI.buttonBodySlantDegrees)
                    textSlantDegrees:
                        (root.numButtons === 1) ? 0 : (((index === 0) ? -0.3 : +0.3) * UI.buttonTextSlantDegrees)
                    labelHorizontalCenterOffset: 0
                    Material.accent:
                        (root.numButtons > 1 && index === 0) ? Qt.lighter(Material.background, 2.0) : root.Material.accent
                    onClicked: root.buttonClicked(index)
                    Keys.onEnterPressed: root.buttonClicked(index)
                    onActiveFocusChanged: {
                        root._isReactingToFocusChanges = true
                        root.buttonActiveFocusChanged(index, activeFocus)
                        root._isReactingToFocusChanges = false
                    }
                }
                onItemAdded: buttonsRepeater.numInstantiatedItems++
                onItemRemoved: buttonsRepeater.numInstantiatedItems--
            }
        }
    }
}