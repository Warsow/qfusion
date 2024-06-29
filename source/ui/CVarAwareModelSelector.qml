import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Rectangle {
    id: root
    color: Qt.rgba(1.0, 1.0, 1.0, 0.025)

    property color modelColor
    property bool fullbright
    property bool drawNativePart
    property bool applyImmediately: true
    property string cvarName
    property string defaultModel

    QtObject {
        id: impl
        property var pendingValue
        property bool suppressSignals: true
        readonly property var models: UI.ui.playerModels
        property int defaultModelIndex: -1
        property int selectedIndex
        property string selectedValue

        onSelectedIndexChanged: {
            impl.selectedValue = impl.models[impl.selectedIndex]
            if (!impl.suppressSignals) {
                if (root.applyImmediately) {
                    UI.ui.setCVarValue(root.cvarName, impl.selectedValue)
                } else {
                    impl.pendingValue = impl.selectedValue
                }
            }
        }

        function mapValueToIndex(value) {
            for (let i = 0; i < impl.models.length; ++i) {
                if (impl.models[i] == value) {
                    return i
                }
            }
            return impl.defaultModelIndex
        }

        function getActualIndex() {
            return impl.mapValueToIndex(UI.ui.getCVarValue(root.cvarName).toLowerCase())
        }
    }

    ToolButton {
        width: 56; height: 56
        anchors.horizontalCenter: parent.left
        anchors.verticalCenter: parent.verticalCenter
        icon.source: "qrc:/ModelLeftArrow.svg"
        onHoveredChanged: {
            if (hovered) {
                UI.ui.playHoverSound()
            }
        }
        onClicked: {
            UI.ui.playSwitchSound()
            impl.selectedIndex = impl.selectedIndex ? impl.selectedIndex - 1 : impl.models.length - 1
        }
    }

    NativelyDrawnModel {
        visible: drawNativePart
        anchors.fill: parent
        modelName: "models/players/" + impl.models[impl.selectedIndex] + "/tris.iqm"
        skinName: "models/players/" + impl.models[impl.selectedIndex] + (root.fullbright ? "/fullbright" : "/default")
        viewOrigin: Qt.vector3d(64.0, 0, 0.0)
        modelOrigin: Qt.vector3d(0.0, 0.0, -16.0)
        desiredModelHeight: 56.0
        rotationSpeed: -90.0
        modelColor: root.modelColor
        outlineColor: root.modelColor
        outlineHeight: 0.33
    }

    ToolButton {
        width: 56; height: 56
        anchors.horizontalCenter: parent.right
        anchors.verticalCenter: parent.verticalCenter
        icon.source: "qrc:/ModelRightArrow.svg"
        onHoveredChanged: {
            if (hovered) {
                UI.ui.playHoverSound()
            }
        }
        onClicked: {
            UI.ui.playSwitchSound()
            impl.selectedIndex = impl.selectedIndex + 1 < impl.models.length ? impl.selectedIndex + 1 : 0
        }
    }

    UILabel {
        anchors.top: parent.top
        anchors.topMargin: 8
        anchors.horizontalCenter: parent.horizontalCenter
        font.weight: Font.Bold
        font.letterSpacing: 1.75
        font.capitalization: Font.Capitalize
        style: Text.Raised
        text: impl.selectedValue
    }

    Connections {
        target: UI.ui
        onCheckingCVarChangesRequested: {
            const index = impl.getActualIndex()
            if (!root.applyImmediately && typeof(impl.pendingValue) !== "undefined") {
                if (impl.mapValueToIndex(impl.pendingValue.toLowerCase()) === index) {
                    impl.pendingValue = undefined
                }
            }
            if (index !== impl.selectedIndex) {
                if (root.applyImmediately || typeof(impl.pendingValue) === "undefined") {
                    impl.suppressSignals = true
                    impl.selectedIndex   = index
                    impl.suppressSignals = false
                }
            }
        }
        onReportingPendingCVarChangesRequested: {
            if (typeof(impl.pendingValue) !== "undefined") {
                UI.ui.reportPendingCVarChanges(root.cvarName, impl.pendingValue)
            }
        }
        onRollingPendingCVarChangesBackRequested: {
            impl.selectedIndex = impl.getActualIndex()
            impl.pendingValue  = undefined
        }
        onPendingCVarChangesCommitted: {
            impl.pendingValue = undefined
        }
    }

    Component.onCompleted: {
        impl.defaultModelIndex = impl.mapValueToIndex(root.defaultModel)
        console.assert(impl.defaultModelIndex >= 0, "Failed to find an index for a default model")
        impl.selectedIndex     = impl.getActualIndex()
        impl.selectedValue     = impl.models[impl.selectedIndex]
        impl.suppressSignals   = false
    }
}