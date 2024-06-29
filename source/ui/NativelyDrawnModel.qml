import QtQuick 2.12
import QtQuick.Controls 2.12
import net.warsow 2.6

Item {
    id: root

    property int nativeZ
    property string modelName
    property string skinName
    property vector3d modelOrigin
    property real rotationSpeed: 0.0
    property vector3d viewOrigin
    property real viewFov: 90.0
    property real desiredModelHeight: 0.0
    property real outlineHeight: 0.0
    property color modelColor: "white"
    property color outlineColor: "black"

    implicitWidth: underlying.isLoaded ? 0 : 192
    implicitHeight: underlying.isLoaded ? 0 : 192

    NativelyDrawnModel_Native {
        id: underlying
        nativeZ: root.nativeZ
        modelName: root.modelName
        skinName: root.skinName
        modelOrigin: root.modelOrigin
        rotationSpeed: root.rotationSpeed
        viewOrigin: root.viewOrigin
        viewFov: root.viewFov
        desiredModelHeight: root.desiredModelHeight
        outlineHeight: root.outlineHeight
        modelColor: root.modelColor
        outlineColor: root.outlineColor
        width: root.width
        height: root.height
        anchors.centerIn: parent

        Connections {
            target: UI.ui
            onNativelyDrawnItemsRetrievalRequested: UI.ui.supplyNativelyDrawnItem(underlying)
        }
    }

    Loader {
        anchors.fill: parent
        sourceComponent: UI.ui.isDebuggingNativelyDrawnItems ? debuggingPlaceholder : null
    }

    Component {
        id: debuggingPlaceholder

        Rectangle {
            anchors.fill: parent
            color: "orange"
            opacity: 0.125
            border.width: 1
            border.color: "red"

            UILabel {
                anchors.centerIn: parent
                wrapMode: Text.NoWrap
                text: skinName.length > 0 ? modelName + " " + skinName : modelName
            }
        }
    }
}