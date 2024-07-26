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
        introLoader.item, mainMenuLoader.item, connectionScreenLoader.item, demoPlaybackMenuLoader.item, inGameMenuLoader.item
    ]

    Item {
        id: shaderSourceItem
        anchors.fill: parent

        property real horizontalRadius: rootItem.width
        property real tintAlpha: 0.03

        property color baseTintColor: (UI.ui.isShowingIntroScreen || (UI.ui.isShowingMainMenu && UI.ui.isClientDisconnected)) ?
            Material.accent : (UI.ui.isShowingConnectionScreen ? "white" : Qt.lighter(Material.background, 1.5))
        Behavior on baseTintColor { ColorAnimation { duration: 100 } }

        readonly property color gradientCentralColor:
            Qt.tint(Material.background, UI.ui.colorWithAlpha(baseTintColor, shaderSourceItem.tintAlpha))
        property real gradientCentralOpacity: UI.ui.isShowingScoreboard ? UI.fullscreenOverlayOpacity : 0.99
        Behavior on gradientCentralOpacity { SmoothedAnimation { duration: 100 } }

        readonly property color gradientSideColor: Qt.darker(Material.background, 1.2)
        // The gradient makes it look denser so the base value is slightly lower
        readonly property real gradientSideOpacity: UI.fullscreenOverlayOpacity - 0.1

        property real expansionFrac: UI.ui.isShowingMainMenu ?
            mainMenuLoader.item.expansionFrac : (UI.ui.isShowingScoreboard ? 1.0 : 0.0)

        SequentialAnimation {
            running: true
            loops: Animation.Infinite
            ParallelAnimation {
                NumberAnimation {
                    target: shaderSourceItem
                    property: "horizontalRadius"
                    from: rootItem.width
                    to: 0.33 * rootItem.width
                    duration: 7500
                }
                NumberAnimation {
                    target: shaderSourceItem
                    property: "tintAlpha"
                    from: 0.03
                    to: 0.05
                    duration: 7500
                }
            }
            ParallelAnimation {
                NumberAnimation {
                    target: shaderSourceItem
                    property: "horizontalRadius"
                    from: 0.33 * rootItem.width
                    to: rootItem.width
                    duration: 7500
                }
                NumberAnimation {
                    target: shaderSourceItem
                    property: "tintAlpha"
                    from: 0.05
                    to: 0.03
                    duration: 7500
                }
            }
        }

        Component {
            id: gradientComponent
            RadialGradient {
                id: radialGradient
                anchors.fill: parent
                horizontalRadius: parent.width
                // Stretches it vertically making it almost a column in the expanded state
                verticalRadius: parent.height * (1.0 + 4.0 * shaderSourceItem.expansionFrac)
                gradient: Gradient {
                    GradientStop {
                        position: 0.00
                        color: UI.ui.colorWithAlpha(shaderSourceItem.gradientCentralColor, shaderSourceItem.gradientCentralOpacity)
                    }
                    GradientStop {
                        position: 1.00
                        color: UI.ui.colorWithAlpha(shaderSourceItem.gradientSideColor, shaderSourceItem.gradientSideOpacity)
                    }
                }
            }
        }

        Loader {
            anchors.fill: parent
            active: !UI.ui.isShowingDemoPlaybackMenu || UI.ui.isShowingScoreboard
            sourceComponent: gradientComponent
        }

        // Force redrawing stuff every frame
        ProgressBar {
            anchors.bottom: parent.bottom
            anchors.left: parent.left
            anchors.right: parent.right
            indeterminate: true
            Material.accent: parent.Material.background
        }

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

        Loader {
            id: introLoader
            active: UI.ui.isShowingIntroScreen
            anchors.fill: parent
            sourceComponent: IntroScreen {}
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