import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

UITextField {
    id: root

    Material.theme: Material.Dark
    Material.accent: "orange"

    property string cvarName: ""
    property bool applyImmediately: true

    QtObject {
        id: impl
        property var pendingValue
    }

    Connections {
        target: UI.ui
        onCheckingCVarChangesRequested: {
            const actualValue = UI.ui.getCVarValue(cvarName)
            if (!applyImmediately && typeof(impl.pendingValue) !== "undefined") {
                if (impl.pendingValue === actualValue) {
                    impl.pendingValue = undefined
                }
            }
            if (actualValue != text) {
                if (applyImmediately || typeof(impl.pendingValue) === "undefined") {
                    text = actualValue
                }
            }
        }
        onReportingPendingCVarChangesRequested: {
            if (typeof(impl.pendingValue) !== "undefined") {
                UI.ui.reportPendingCVarChanges(cvarName, impl.pendingValue)
            }
        }
        onRollingPendingCVarChangesBackRequested: {
            root.text         = UI.ui.getCVarValue(cvarName)
            impl.pendingValue = undefined
        }
        onPendingCVarChangesCommitted: {
            impl.pendingValue = undefined
        }
    }

    onTextEdited: {
        if (applyImmediately) {
            UI.ui.setCVarValue(cvarName, text)
        } else {
            impl.pendingValue = text
        }
    }

    Component.onCompleted: {
        text = UI.ui.getCVarValue(cvarName)
    }
}