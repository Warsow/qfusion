import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

UISlider {
    id: root

    Material.theme: Material.Dark
    Material.accent: "orange"

    property string cvarName: ""
    property bool applyImmediately: true

    QtObject {
        id: impl
        property bool suppressSignals: true
        property var pendingValue

        function getCurrNumericVarValue() {
            const textValue = UI.ui.getCVarValue(root.cvarName)
            let parsedValue = parseFloat(textValue)
            if (isNaN(parsedValue)) {
                parsedValue = 0.0
            }
            return Math.max(root.from, Math.min(parsedValue, root.to))
        }
    }

    Connections {
        target: UI.ui
        onCheckingCVarChangesRequested: {
            const newValue = impl.getCurrNumericVarValue()
            if (!applyImmediately && typeof(impl.pendingValue) !== "undefined") {
                if (impl.pendingValue === newValue) {
                    impl.pendingValue = undefined
                }
            }
            if (root.value != newValue) {
                if (applyImmediately || typeof(impl.pendingValue) === "undefined") {
                    console.assert(!impl.suppressSignals)
                    impl.suppressSignals = true
                    root.value           = newValue
                    impl.suppressSignals = false
                }
            }
        }
        onReportingPendingCVarChangesRequested: {
            if (typeof(impl.pendingValue) !== "undefined") {
                UI.ui.reportPendingCVarChanges(cvarName, impl.pendingValue)
            }
        }
        onRollingPendingCVarChangesBackRequested: {
            impl.suppressSignals = true
            root.value           = impl.getCurrNumericVarValue()
            impl.pendingValue    = undefined
            impl.suppressSignals = false
        }
        onPendingCVarChangesCommitted: {
            impl.pendingValue = undefined
        }
    }

    onValueChanged: {
        if (!impl.suppressSignals) {
            if (applyImmediately) {
                UI.ui.setCVarValue(cvarName, value)
            } else {
                impl.pendingValue = value
            }
        }
    }

    Component.onCompleted: {
        value                = impl.getCurrNumericVarValue()
        impl.suppressSignals = false
    }
}