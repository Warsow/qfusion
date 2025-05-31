import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

AutoFittingComboBox {
    id: root

    property string cvarName: ""
    property bool applyImmediately: true

    property var knownHeadingsAndValues

    readonly property var values: impl.values
    readonly property var headings: impl.headings

    readonly property string placeholder: "(unknown)"

    QtObject {
        id: impl
        property var pendingValue
        property var oldValue
        property bool skipIndexChangeSignal: true
        property var headings
        property var values

        function setNewValue(value) {
            let index = impl.mapValueToIndex(value)
            if (index < 0) {
                // Add a placeholder if needed. Otherwise just update the placeholder value
                if (impl.headings[impl.headings.length - 1] !== placeholder) {
                    impl.headings.push(placeholder)
                    impl.values.push(value)
                    model = impl.headings
                } else {
                    impl.values[impl.values.length - 1] = value
                }
                index = impl.values.length - 1
            } else {
                // Remove the placeholder if it's no longer useful
                if (index != impl.headings.length - 1 && impl.headings[impl.headings.length - 1] === placeholder) {
                    impl.values.pop()
                    impl.headings.pop()
                    model = impl.headings
                }
            }

            impl.oldValue = value
            if (root.currentIndex != index) {
                root.currentIndex = index
            }
        }

        function mapValueToIndex(value) {
            const knownValues = knownHeadingsAndValues[1]
            for (let i = 0; i < knownValues.length; ++i) {
                if (knownValues[i] == value) {
                    return i
                }
            }
            return -1
        }
    }

    onKnownHeadingsAndValuesChanged: {
        const value = UI.ui.getCVarValue(cvarName)
        // Make deep copies
        impl.headings = [...knownHeadingsAndValues[0]]
        impl.values   = [...knownHeadingsAndValues[1]]
        if (impl.mapValueToIndex(value) < 0) {
            impl.values.push(value)
            impl.headings.push(placeholder)
        }
        model = impl.headings
        // Force a value re-check/index update next frame (for some unknown reasons it has to be postponed)
        impl.oldValue = undefined
    }

    Connections {
        target: UI.ui
        onCheckingCVarChangesRequested: {
            const value = UI.ui.getCVarValue(cvarName)
            if (!applyImmediately && typeof(impl.pendingValue) !== "undefined") {
                if ((impl.pendingValue + '') === value) {
                    impl.pendingValue = undefined
                }
            }
            if (value != impl.oldValue) {
                if (applyImmediately || typeof(impl.pendingValue) === "undefined") {
                    impl.skipIndexChangeSignal = true
                    impl.setNewValue(value)
                    impl.skipIndexChangeSignal = false
                }
            }
        }
        onReportingPendingCVarChangesRequested: {
            if (typeof(impl.pendingValue) !== "undefined") {
                UI.ui.reportPendingCVarChanges(cvarName, impl.pendingValue)
            }
        }
        onRollingPendingCVarChangesBackRequested: {
            impl.skipIndexChangeSignal = true
            impl.setNewValue(UI.ui.getCVarValue(cvarName))
            impl.pendingValue          = undefined
            impl.skipIndexChangeSignal = false
        }
        onPendingCVarChangesCommitted: {
            impl.pendingValue = undefined
        }
    }

    onCurrentIndexChanged: {
        if (!impl.skipIndexChangeSignal) {
            const value = impl.values[currentIndex]
            if (applyImmediately) {
                UI.ui.setCVarValue(cvarName, value)
            } else {
                impl.pendingValue = value
            }
        }
    }

    Component.onCompleted: {
        // Values are already set in onKnownHeadingsAndValuesChanged handler
        impl.skipIndexChangeSignal = false
    }
}