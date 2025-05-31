import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

RowLayout {
    id: root
    spacing: 8
    clip: false

    property string cvarName
    property bool applyImmediately: true

    readonly property color selectedColor: impl.selectedColor

    // For embedding in SettingsRow
    readonly property real leftPadding: 0.0
    readonly property real rightPadding: 0.0

    QtObject {
        id: impl

        // We have decided to fall back to procedural code for updates, otherwise things quickly become non-manageable
        property var pendingValue
        property var customColor
        property int selectedIndex: 0
        property var selectedColor

        function toQuakeColorString(color) {
            // Let Qt convert it to "#RRGGBB". This is more robust than a manual floating-point -> 0.255 conversion.
            let qtColorString = '' + color
            let r = parseInt(qtColorString.substring(1, 3), 16)
            let g = parseInt(qtColorString.substring(3, 5), 16)
            let b = parseInt(qtColorString.substring(5, 7), 16)
            return r + " " + g + " " + b
        }

        function indexOfColor(maybeColor) {
            const colors = UI.ui.consoleColors
            // Compare by hex strings
            const givenString = '' + maybeColor
            for (let i = 0; i < colors.length; ++i) {
                if (('' + colors[i]) === givenString) {
                    return i
                }
            }
            return -1
        }

        function getCVarData() {
            const rawString  = UI.ui.getCVarValue(cvarName)
            const maybeColor = UI.ui.colorFromRgbString(rawString)
            if (!maybeColor) {
                // White (TODO: Avoid hardcoding this value)
                return [7, undefined]
            }
            const index = impl.indexOfColor(maybeColor)
            if (index >= 0) {
                return [index, undefined]
            }
            return [UI.ui.consoleColors.length, maybeColor]
        }
    }

    Repeater {
        model: UI.ui.consoleColors

        delegate: ColorPickerColorItem {
            layoutIndex: index
            selected: layoutIndex === impl.selectedIndex
            color: UI.ui.consoleColors[index]
            onMouseEnter: root.expandAt(index)
            onMouseLeave: root.shrinkBack()
            onClicked: {
                UI.ui.playSwitchSound()
                // We do not touch the custom color
                impl.selectedIndex = index
                impl.selectedColor = UI.ui.consoleColors[index]
                updateCVarColor(impl.selectedColor)
            }
        }
    }

    ColorPickerColorItem {
        visible: !!impl.customColor
        layoutIndex: UI.ui.consoleColors.length
        selected: layoutIndex === impl.selectedIndex
        color: impl.customColor ? impl.customColor : "transparent"
        onMouseEnter: root.expandAt(index)
        onMouseLeave: root.shrinkBack()
        onClicked: {
            UI.ui.playHoverSound()
            // We do not touch the custom color
            impl.selectedIndex = UI.ui.consoleColors.length
            impl.selectedColor = impl.customColor
            updateCVarColor(impl.selectedColor)
        }
    }

    ColorPickerItem {
        id: cross
        layoutIndex: UI.ui.consoleColors.length + 1
        haloColor: Material.accentColor
        onMouseEnter: root.expandAt(index)
        onMouseLeave: root.shrinkBack()
        onClicked: popup.openForColor(impl.customColor)

        readonly property color crossColor: containsMouse ? Material.accentColor : Material.foreground

        contentItem: Item {
            anchors.fill: parent
            Rectangle {
                color: cross.crossColor
                anchors.centerIn: parent
                width: parent.width - 2
                height: 4
            }
            Rectangle {
                color: cross.crossColor
                anchors.centerIn: parent
                width: 4
                height: parent.height - 2
            }
        }
    }

    Connections {
        target: UI.ui
        onCheckingCVarChangesRequested: {
            const [index, maybeCustomColor] = impl.getCVarData()
            if (!root.applyImmediately && typeof(impl.pendingValue) !== "undefined") {
                if (maybeCustomColor) {
                    if (impl.toQuakeColorString(maybeCustomColor) === impl.pendingValue) {
                        impl.pendingValue = undefined
                    }
                } else {
                    if (impl.toQuakeColorString(UI.ui.consoleColors[index]) === impl.pendingValue) {
                        impl.pendingValue = undefined
                    }
                }
            }
            if (index !== impl.selectedIndex || maybeCustomColor !== impl.customColor) {
                if (root.applyImmediately || typeof(impl.pendingValue) === "undefined") {
                    impl.customColor   = maybeCustomColor
                    impl.selectedIndex = index
                    impl.selectedColor = maybeCustomColor ? maybeCustomColor : UI.ui.consoleColors[index]
                }
            }
        }
        onReportingPendingCVarChangesRequested: {
            if (typeof(impl.pendingValue) !== "undefined") {
                UI.ui.reportPendingCVarChanges(root.cvarName, impl.pendingValue)
            }
        }
        onRollingPendingCVarChangesBackRequested: {
            const [index, maybeCustomColor] = impl.getCVarData()
            impl.customColor   = maybeCustomColor
            impl.selectedIndex = index
            impl.selectedColor = maybeCustomColor ? maybeCustomColor : UI.ui.consoleColors[index]

            impl.pendingValue = undefined
        }
        onPendingCVarChangesCommitted: {
            impl.pendingValue = undefined
        }
    }

    function expandAt(hoveredIndex) {
        for (let i = 0; i < children.length; ++i) {
            const child = children[i]
            if (child instanceof ColorPickerItem) {
                const childIndex = child.layoutIndex
                if (childIndex !== hoveredIndex) {
                    child.startShift(childIndex < hoveredIndex)
                }
            }
        }
    }

    function shrinkBack() {
        for (let i = 0; i < children.length; ++i) {
            const child = children[i]
            if (child instanceof ColorPickerItem) {
                child.revertShift()
            }
        }
    }

    function updateCVarColor(color) {
        const value = impl.toQuakeColorString(color)
        if (applyImmediately) {
            UI.ui.setCVarValue(cvarName, value)
        } else {
            impl.pendingValue = value
        }
    }

    function setSelectedCustomColor(color) {
        const index = impl.indexOfColor(color)
        if (index < 0) {
            impl.customColor   = color
            impl.selectedIndex = UI.ui.consoleColors.length
            impl.selectedColor = color
        } else {
            impl.customColor   = undefined
            impl.selectedIndex = index
            impl.selectedColor = UI.ui.consoleColors[index]
        }
        updateCVarColor(color)
    }

    Component.onCompleted: {
        const [index, maybeCustomColor] = impl.getCVarData()
        impl.customColor   = maybeCustomColor
        impl.selectedIndex = index
        impl.selectedColor = maybeCustomColor ? maybeCustomColor : UI.ui.consoleColors[index]
    }

    Popup {
        id: popup
        focus: true
        dim: false

        x: root.width - 24
        y: -12
        width: 128 + 64 + 16
        height: 128
        transformOrigin: Item.Top
        topMargin: 12
        bottomMargin: 12
        verticalPadding: 8

        property var selectedColor
        property bool hasChanges: false

        function openForColor(customColor) {
            rSlider.value = customColor ? customColor.r : 1.0
            gSlider.value = customColor ? customColor.g : 1.0
            bSlider.value = customColor ? customColor.b : 1.0
            popup.selectedColor = customColor
            popup.hasChanges    = false
            popup.open()
        }

        function updateSelectedColor(r, g, b) {
            if (opened) {
                // Don't modify the selected color partially
                const newColor = Qt.rgba(r, g, b, 1.0)
                if (selectedColor != newColor) {
                    selectedColor = newColor
                    hasChanges = true
                }
            }
        }

        background: Rectangle {
            radius: 5
            color: Material.background
            opacity: 0.67

            layer.enabled: true
            layer.effect: ElevationEffect { elevation: 16 }
        }

        onVisibleChanged: repositionBlurRegion()
        onXChanged: repositionBlurRegion()
        onYChanged: repositionBlurRegion()
        onWidthChanged: repositionBlurRegion()
        onHeightChanged: repositionBlurRegion()

        onAboutToHide: {
            rootItem.resetPopupMode()
        }

        function repositionBlurRegion() {
            if (popup.visible) {
                const globalPos = parent.mapToGlobal(0, 0)
                const inset     = 2

                rootItem.setOrUpdatePopupMode(Qt.rect(globalPos.x + inset + popup.x, globalPos.y + inset + popup.y,
                    background.width - 2 * inset, background.height - 2 * inset))
            }
        }

        contentItem: ColumnLayout {
            Material.theme: Material.Dark
            spacing: 8

            RowLayout {
                Layout.fillWidth: true
                Text {
                    Layout.leftMargin: 2
                    Layout.preferredWidth: 24
                    color: rSlider.Material.accent
                    text: Math.round(rSlider.value * 255)
                    font.weight: Font.Medium
                    style: Text.Raised
                }
                UISlider {
                    id: rSlider
                    Layout.fillHeight: true
                    Layout.fillWidth: true
                    Material.accent: Qt.rgba(1.0, 0.0, 0.0, 1.0)
                    Material.foreground: Material.accent
                    onValueChanged: popup.updateSelectedColor(rSlider.value, gSlider.value, bSlider.value)
                }
            }

            RowLayout {
                Layout.fillWidth: true
                Text {
                    Layout.leftMargin: 2
                    Layout.preferredWidth: 24
                    color: gSlider.Material.accent
                    text: Math.round(gSlider.value * 255)
                    font.weight: Font.Medium
                    style: Text.Raised
                }
                UISlider {
                    id: gSlider
                    Layout.fillHeight: true
                    Layout.fillWidth: true
                    Material.accent: Qt.rgba(0.0, 1.0, 0.0, 1.0)
                    onValueChanged: popup.updateSelectedColor(rSlider.value, gSlider.value, bSlider.value)
                }
            }

            RowLayout {
                Layout.fillWidth: true
                Text {
                    Layout.leftMargin: 2
                    Layout.preferredWidth: 24
                    color: bSlider.Material.accent
                    text: Math.round(bSlider.value * 255)
                    font.weight: Font.Medium
                    style: Text.Raised
                }
                UISlider {
                    id: bSlider
                    Layout.fillHeight: true
                    Layout.fillWidth: true
                    Material.accent: Qt.rgba(0.0, 0.0, 1.0, 1.0)
                    onValueChanged: popup.updateSelectedColor(rSlider.value, gSlider.value, bSlider.value)
                }
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.topMargin: -8
                Layout.bottomMargin: -8

                RoundButton {
                    Layout.leftMargin: -4
                    flat: true
                    text: "\u2714"
                    font.family: UI.ui.symbolsFontFamily
                    enabled: !!popup.selectedColor && popup.hasChanges
                    onClicked: {
                        UI.ui.playSwitchSound()
                        root.setSelectedCustomColor(popup.selectedColor)
                        popup.close()
                    }
                    onHoveredChanged: {
                        if (hovered) {
                            UI.ui.playHoverSound()
                        }
                    }
                }

                Rectangle {
                    Layout.alignment: Qt.AlignHCenter | Qt.AlignVCenter
                    Layout.fillWidth: true
                    Layout.leftMargin: 4
                    Layout.preferredHeight: 12
                    radius: 3
                    color: popup.selectedColor ? popup.selectedColor : "transparent"
                    border.width: popup.selectedColor ? 0 : 1
                    border.color: Qt.rgba(0.5, 0.5, 0.5, 0.25)
                }

                RoundButton {
                    Layout.rightMargin: -4
                    flat: true
                    text: "\u2716"
                    font.family: UI.ui.symbolsFontFamily
                    onClicked: {
                        UI.ui.playSwitchSound();
                        popup.close()
                    }
                    onHoveredChanged: {
                        if (hovered) {
                            UI.ui.playHoverSound()
                        }
                    }
                }
            }
        }
    }
}