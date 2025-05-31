import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtMultimedia 5.12
import net.warsow 2.6

Item {
    id: root
    property string filePath
    property var stubImagePath
    property color overlayColor: Material.background
    property color stubColor: "white"
    property real videoOpacity: 0.5
    property real overlayOpacity: 0.1
    property real stubImageOpacity: 0.5
    property real stubRectangleOpacity: 0.1

    WswVideoSource {
        id: videoSource
        filePath: root.filePath
    }

    Component {
        id: outputComponent
        VideoOutput {
            width: root.width
            height: root.height
            source: videoSource
            opacity: 0.0
            NumberAnimation on opacity {
                running: true
                to: videoOpacity
                duration: 33
            }
        }
    }

    Component {
        id: stubImageComponent
        Image {
            width: root.width
            height: root.height
            fillMode: Image.PreserveAspectCrop
            smooth: true
            mipmap: true
            source: stubImagePath
            opacity: 0.0
            NumberAnimation on opacity {
                to: stubImageOpacity
                duration: 33
            }
        }
    }

    Component {
        id: stubRectangleComponent
        Rectangle {
            width: root.width
            height: root.height
            color: stubColor
            opacity: 0.0
            NumberAnimation on opacity {
                running: true
                to: stubRectangleOpacity
                duration: 33
            }
        }
    }

    Loader {
        anchors.fill: parent
        sourceComponent:
            videoSource.status === WswVideoSource.Running ? outputComponent :
                (!!stubImagePath ? stubImageComponent : stubRectangleComponent)
    }

    Rectangle {
        anchors.fill: parent
        visible: overlayOpacity > 0.0
        color: overlayColor
        opacity: overlayOpacity
    }
}