import QtQuick 2.12
import QtQuick.Controls 2.12
import net.warsow 2.6

Item {
    id: root

    readonly property size sourceSize: underlying.sourceSize
    property string materialName: ""
    property size desiredSize
    property int borderWidth: 0
    property bool useOutlineEffect: false
    property bool fitSizeForCrispness: false
    property color color: "white"
    property int nativeZ: 0

    implicitWidth: sourceSize.width ? sourceSize.width : 192
    implicitHeight: sourceSize.height ? sourceSize.height: 192

    clip: false

    NativelyDrawnImage_Native {
        id: underlying
        materialName: root.materialName
        desiredSize: root.desiredSize
        borderWidth: root.borderWidth
        useOutlineEffect: root.useOutlineEffect
        fitSizeForCrispness: root.fitSizeForCrispness
        opacity: root.opacity
        color: root.color
        nativeZ: root.nativeZ
        width: parent.width
        height: parent.height
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
            color: underlying.isLoaded ? "red" : "transparent"
            opacity: 0.125
            border.width: 1
            border.color: "orange"

            UILabel {
                anchors.centerIn: parent
                wrapMode: Text.NoWrap
                text: underlying.materialName
            }
        }
    }
}