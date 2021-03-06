import QtQuick 2.12
import QtQuick.Controls 2.12
import net.warsow 2.6

Item {
    id: root

    readonly property size sourceSize: underlying.sourceSize
    property string materialName: ""
    property int nativeZ: 0

    implicitWidth: sourceSize.width ? sourceSize.width : 192
    implicitHeight: sourceSize.height ? sourceSize.height: 192

    clip: false

    NativelyDrawnImage_Native {
        id: underlying
        materialName: root.materialName
        nativeZ: root.nativeZ
        width: parent.width
        height: parent.height
        anchors.centerIn: parent

        Component.onCompleted: wsw.registerNativelyDrawnItem(underlying)
        Component.onDestruction: wsw.unregisterNativelyDrawnItem(underlying)
    }

    Loader {
        anchors.fill: parent
        sourceComponent: underlying.isLoaded && !wsw.isDebuggingNativelyDrawnItems ? null : debuggingPlaceholder
    }

    Component {
        id: debuggingPlaceholder

        Rectangle {
            anchors.fill: parent
            color: "red"
            opacity: 0.125
            border.width: 1
            border.color: "orange"

            Label {
                anchors.centerIn: parent
                wrapMode: Text.NoWrap
                font.pointSize: 12
                text: underlying.materialName
            }
        }
    }
}