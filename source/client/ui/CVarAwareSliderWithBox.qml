import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root

    implicitWidth: slider.implicitWidth + 16
    implicitHeight: slider.implicitHeight

    property string cvarName: ""
    property bool applyImmediately: true

    // For embedding in SettingsRow
    readonly property real leftPadding: 0.0
    readonly property real rightPadding: 0.0

    QtObject {
        id: impl
        property bool suppressSignals: true
        property var pendingValue

        function getCurrNumericVarValue() {
            return impl.textToValue(UI.ui.getCVarValue(root.cvarName))
        }

        function valueToText(v) {
            return (v + 0.0).toFixed(root.fractionalPartDigits)
        }

        function textToValue(text) {
            let v = parseFloat(text)
            if (isNaN(v)) {
                v = 0.0
            }
            return Math.max(slider.from, Math.min(v, slider.to))
        }
    }

    property int fractionalPartDigits: 2
    property real textFieldWidth: 56

    property bool putBoxToTheLeft: true

    property alias from: slider.from
    property alias to: slider.to
    property alias stepSize: slider.stepSize

    property real value

    states: [
        State {
            name: "boxToTheLeft"
            when: putBoxToTheLeft
            PropertyChanges {
                target: slider
                anchors.left: undefined
                anchors.right: root.right
            }
            PropertyChanges {
                target: textField
                anchors.left: root.left
                anchors.right: undefined
            }
        },
        State {
            name: "boxToTheRight"
            when: !putBoxToTheLeft
            PropertyChanges {
                target: slider
                anchors.left: root.left
                anchors.right: undefined
            }
            PropertyChanges {
                target: textField
                anchors.left: undefined
                anchors.right: root.right
            }
        }
    ]

    onValueChanged: {
        if (value !== slider.value) {
            const suppressSliderSignals = slider.suppressSignals
            slider.suppressSignals      = true
            slider.value                = value
            slider.suppressSignals      = suppressSliderSignals
        }

        const text = impl.valueToText(value)
        if (text != textField.text) {
            const suppressFieldSignals = textField.suppressSignals
            textField.suppressSignals  = true
            textField.text             = text
            textField.suppressSignals  = suppressFieldSignals
        }

        if (!impl.suppressSignals) {
            if (applyImmediately) {
                UI.ui.setCVarValue(cvarName, value)
            } else {
                impl.pendingValue = value
            }
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
            if (value != newValue) {
                if (applyImmediately || typeof(impl.pendingValue) === "undefined") {
                    value = newValue
                }
            }
        }
        onReportingPendingCVarChangesRequested: {
            if (typeof(impl.pendingValue) !== "undefined") {
                UI.ui.reportPendingCVarChanges(root.cvarName, impl.pendingValue)
            }
        }
        onRollingPendingCVarChangesBackRequested: {
            impl.suppressSignals      = true
            slider.suppressSignals    = true
            textField.suppressSignals = true

            root.value        = impl.getCurrNumericVarValue()
            impl.pendingValue = undefined

            textField.suppressSignals = false
            slider.suppressSignals    = false
            impl.suppressSignals      = false
        }
        onPendingCVarChangesCommitted: {
            impl.pendingValue = undefined
        }
    }

    Component.onCompleted: {
        console.assert(impl.suppressSignals)
        console.assert(slider.suppressSignals)
        console.assert(textField.suppressSignals)
        root.value                = impl.getCurrNumericVarValue()
        slider.value              = root.value
        textField.text            = impl.valueToText(root.value)
        impl.suppressSignals      = false
        slider.suppressSignals    = false
        textField.suppressSignals = false
    }

    UISlider {
        id: slider
        width: root.implicitWidth - textField.width - 8

        anchors.verticalCenter: parent.verticalCenter

        Material.theme: Material.Dark
        Material.accent: "orange"

        property bool suppressSignals: true

        onValueChanged: {
            if (!slider.suppressSignals) {
                if (root.value !== slider.value) {
                    root.value = slider.value
                }
            }
        }
    }

    UITextField {
        id: textField

        anchors.verticalCenter: slider.verticalCenter
        anchors.verticalCenterOffset: +2
        width: textFieldWidth
        horizontalAlignment: Qt.AlignHCenter
        verticalAlignment: Qt.AlignHCenter
        maximumLength: 3 + fractionalPartDigits
        selectByMouse: false

        property bool suppressSignals: true

        background: Rectangle {
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.verticalCenter: parent.verticalCenter
            anchors.verticalCenterOffset: -4
            width: parent.width
            height: 34
            radius: 1
            color: "transparent"
            border.color: parent.activeFocus ? Material.accent : Material.foreground
            border.width: 1
            opacity: parent.activeFocus ? 1.0 : 0.1
        }

        onEditingFinished: {
            if (!textField.suppressSignals) {
                const numericValue = impl.textToValue(textField.text)
                const newTextValue = impl.valueToText(numericValue)
                if (newTextValue !== textField.text) {
                    textField.suppressSignals = true
                    textField.text            = newTextValue
                    textField.suppressSignals = false
                }
                if (root.value !== numericValue) {
                    root.value = numericValue
                }
            }
        }
    }
}