import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

UICheckBox {
    id: root

    Material.foreground: "white"
    Material.accent: "orange"

    property string cvarName: ""
    property bool applyImmediately: true

    QtObject {
        id: impl
        property var pendingChecked
    }

    Connections {
        target: UI.ui
        onCheckingCVarChangesRequested: {
            const value = fromNative(UI.ui.getCVarValue(cvarName))
            if (!applyImmediately && typeof(impl.pendingChecked) !== "undefined") {
                if (impl.pendingChecked === value) {
                    impl.pendingChecked = undefined
                }
            }
            if (checked != value) {
                if (applyImmediately || typeof(impl.pendingChecked) === "undefined") {
                    checked = value
                }
            }
        }
        onReportingPendingCVarChangesRequested: {
            if (typeof(impl.pendingChecked) !== "undefined") {
                UI.ui.reportPendingCVarChanges(cvarName, toNative(impl.pendingChecked))
            }
        }
        onRollingPendingCVarChangesBackRequested: {
            checked             = fromNative(UI.ui.getCVarValue(cvarName))
            impl.pendingChecked = undefined
        }
        onPendingCVarChangesCommitted: {
            impl.pendingChecked = undefined
        }
    }

    function fromNative(value) { return value != 0; }
    function toNative(value) { return value ? "1" : "0"; }

    onClicked: {
        if (applyImmediately) {
            UI.ui.setCVarValue(cvarName, toNative(checked))
        } else {
            impl.pendingChecked = checked
        }
    }

    Component.onCompleted: checked = fromNative(UI.ui.getCVarValue(cvarName))
}