import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Window 2.12
import QtGraphicalEffects 1.12
import net.warsow 2.6

Item {
    id: rootItem

    property var windowContentItem

    property bool isBlurringBackground
    property bool isBlurringLimitedToRect
    property rect blurRect

    Window.onWindowChanged: {
        if (Window.window) {
            Window.window.requestActivate()
            rootItem.forceActiveFocus()
            rootItem.windowContentItem = Window.window.contentItem
        }
    }

    Keys.forwardTo: [
        mainMenuLoader.item, connectionScreenLoader.item, demoPlaybackMenuLoader.item, inGameMenuLoader.item
    ]

    Item {
        id: shaderSourceItem
        anchors.fill: parent

        Loader {
            id: mainMenuLoader
            active: UI.ui.isShowingMainMenu
            anchors.fill: parent
            sourceComponent: MainMenu {}
        }

        Loader {
            id: connectionScreenLoader
            active: UI.ui.isShowingConnectionScreen
            anchors.fill: parent
            sourceComponent: ConnectionScreen {}
        }

        Loader {
            id: inGameMenuLoader
            active: UI.ui.isShowingInGameMenu
            anchors.fill: parent
            sourceComponent: InGameMenu {}
        }

        Loader {
            active: UI.ui.isShowingScoreboard
            anchors.fill: parent
            sourceComponent: ScoreboardScreen {}
        }

        Loader {
            id: demoPlaybackMenuLoader
            active: UI.ui.isShowingDemoPlaybackMenu
            anchors.fill: parent
            sourceComponent: DemoPlaybackMenu {}
        }
    }

    ShaderEffectSource {
        id: effectSource
        visible: false
        hideSource: isBlurringBackground && !isBlurringLimitedToRect
        x: blurRect.x
        y: blurRect.y
        width: blurRect.width
        height: blurRect.height
        sourceItem: shaderSourceItem
        sourceRect: blurRect
    }

    FastBlur {
        visible: isBlurringBackground
        anchors.fill: effectSource
        source: effectSource
        radius: 32
    }

    MouseArea {
        id: popupOverlay
        visible: isBlurringBackground && !isBlurringLimitedToRect
        hoverEnabled: true
        anchors.fill: parent
    }

    function setOrUpdatePopupMode(limitToItem) {
        if (limitToItem) {
            isBlurringLimitedToRect = true
            blurRect                = Qt.rect(limitToItem.x, limitToItem.y, limitToItem.width, limitToItem.height)
        } else {
            isBlurringLimitedToRect = false
            blurRect = Qt.rect(0, 0, rootItem.width, rootItem.height)
        }
        isBlurringBackground = true
    }

    function resetPopupMode() {
        isBlurringBackground    = false
        isBlurringLimitedToRect = false
        blurRect                = Qt.rect(0, 0, 0, 0)
        // TODO: Try tracking Window::activeFocusItem?
        rootItem.forceActiveFocus()
    }
}