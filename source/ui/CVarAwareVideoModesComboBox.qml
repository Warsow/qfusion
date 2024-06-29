import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

AutoFittingComboBox {
    id: root

    model: impl.headings

    QtObject {
        id: impl

        property int oldWidth
        property int oldHeight
        property bool skipIndexChangeSignal: true
        property var pendingWidthAndHeight

        readonly property var knownWidthValues: UI.ui.videoModeWidthValuesList
        readonly property var knownHeightValues: UI.ui.videoModeHeightValuesList

        property var headings: UI.ui.videoModeHeadingsList
        property var widthValues: knownWidthValues
        property var heightValues: knownHeightValues

        function parse(stringValue) {
            const parsedValue = parseInt(stringValue, 10)
            return !isNaN(parsedValue) ? parsedValue : 0
        }

        function getActualWidth() { return impl.parse(UI.ui.getCVarValue("vid_width")) }
        function getActualHeight() { return impl.parse(UI.ui.getCVarValue("vid_height")) }

        function setNewValues(width, height) {
            let index = impl.mapValuesToIndex(width, height)
            if (index < 0) {
                // Add a placeholder if needed. Otherwise just update the placeholder value
                if (impl.headings[impl.headings.length - 1] !== "(custom)") {
                    impl.headings.push("(custom)")
                    impl.widthValues.push(width)
                    impl.heightValues.push(height)
                    root.model = impl.headings
                } else {
                    impl.widthValues[impl.widthValues.length - 1]   = width
                    impl.heightValues[impl.heightValues.length - 1] = height
                }
                index = impl.widthValues.length - 1
            } else {
                // Remove the placeholder if it's no longer useful
                if (index != impl.headings.length - 1 && impl.headings[impl.headings.length - 1] === "(custom)") {
                    impl.widthValues.pop()
                    impl.heightValues.pop()
                    impl.headings.pop()
                    model = impl.headings
                }
            }

            impl.oldWidth  = width
            impl.oldHeight = height
            if (root.currentIndex != index) {
                root.currentIndex = index
            }
        }

        function mapValuesToIndex(width, height) {
            for (let i = 0; i < impl.knownWidthValues.length; ++i) {
                if (impl.knownWidthValues[i] === width && impl.knownHeightValues[i] === height) {
                    return i
                }
            }
            return -1
        }
    }

    Connections {
        target: UI.ui
        onCheckingCVarChangesRequested: {
            const width  = impl.getActualWidth()
            const height = impl.getActualHeight()
            if (typeof(impl.pendingWidthAndHeight) !== "undefined") {
                // TODO: Should we check latched values instead?
                if (impl.pendingWidthAndHeight[0] === width && impl.pendingWidthAndHeight[1] === height) {
                    impl.pendingWidthAndHeight = undefined
                }
            }
            if (width !== impl.oldWidth || height !== impl.oldHeight) {
                if (typeof(impl.pendingWidthAndHeight) === "undefined") {
                    impl.setNewValues(width, height)
                }
            }
        }
        onReportingPendingCVarChangesRequested: {
            if (typeof(impl.pendingWidthAndHeight) !== "undefined") {
                UI.ui.reportPendingCVarChanges("vid_width", impl.pendingWidthAndHeight[0])
                UI.ui.reportPendingCVarChanges("vid_height", impl.pendingWidthAndHeight[1])
            }
        }
        onRollingPendingCVarChangesBackRequested: {
            impl.skipIndexChangeSignal = true
            impl.setNewValues(impl.getActualWidth(), impl.getActualHeight())
            impl.pendingWidthAndHeight = undefined
            impl.skipIndexChangeSignal = false
        }
        onPendingCVarChangesCommitted: {
            impl.pendingWidthAndHeight = undefined
        }
    }

    onCurrentIndexChanged: {
        if (!impl.skipIndexChangeSignal) {
            impl.pendingWidthAndHeight = [impl.widthValues[currentIndex], impl.heightValues[currentIndex]]
        }
    }

    Component.onCompleted: {
        console.assert(impl.heightValues.length === impl.widthValues.length)
        console.assert(impl.headings.length === impl.widthValues.length)
        impl.setNewValues(impl.getActualWidth(), impl.getActualHeight())
        impl.skipIndexChangeSignal = false
    }
}