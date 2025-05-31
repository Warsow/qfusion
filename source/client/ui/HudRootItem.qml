import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Window 2.12
import QtGraphicalEffects 1.12
import net.warsow 2.6

Item {
    id: rootItem

    property var instantiatedMiniviewHuds: ({})

    property var oldMiniviews: []
    property var oldMiniviewIndices: []

    readonly property bool suppressShowingTileHuds: Hud.ui.isShowingScoreboard || Hud.ui.isShowingInGameMenu || Hud.ui.isShowingMainMenu

    onSuppressShowingTileHudsChanged: updateVisibilityOfTileHuds()

    Window.onWindowChanged: {
        if (Window.window) {
            Window.window.requestActivate()
            rootItem.forceActiveFocus()
        }
    }

    // Try reusing the same instance due to Qml GC quirks
    InGameHud {
        // TODO: Is visibility switching it really needed (we don't draw it anyway, but property updates handling may vary)?
        visible: Hud.ui.isShowingHud
        anchors.fill: parent
        layoutModel: Hud.commonDataModel.regularLayoutModel
        commonDataModel: Hud.commonDataModel
        povDataModel: Hud.povDataModel
        miniviewAllocator: rootItem
        // Ensure that it is on top of miniviews and their huds (the primary hud contains chat popups along with other things)
        z: +1
    }

    Component.onCompleted: {
        applyMiniviewLayoutPass1()
        applyMiniviewLayoutPass2()
    }

    Connections {
        target: Hud.commonDataModel
        onMiniviewLayoutChangedPass1: applyMiniviewLayoutPass1()
        onMiniviewLayoutChangedPass2: applyMiniviewLayoutPass2()
    }

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: {
            for (const view of oldMiniviews) {
                Hud.ui.supplyDisplayedHudItemAndMargin(view, 4.0)
            }
        }
        onShuttingDown: {
            for (const view of Object.values(rootItem.instantiatedMiniviewHuds)) {
                view.parent = null
                view.destroy()
            }
            rootItem.instantiatedMiniviewHuds = null
        }
    }

    function applyMiniviewLayoutPass1() {
        // Recycle all (we have to care of preserving the original order)
        for (let i = 0; i < oldMiniviews.length; ++i) {
            recycle(oldMiniviews[i], oldMiniviewIndices[i])
        }
        oldMiniviews.length       = 0
        oldMiniviewIndices.length = 0
    }

    function applyMiniviewLayoutPass2() {
        // Take all needed
        const miniviewIndices = Hud.commonDataModel.getFixedPositionMiniviewIndices()
        for (const miniviewIndex of miniviewIndices) {
            const view = take(miniviewIndex)
            oldMiniviewIndices.push(miniviewIndex)
            oldMiniviews.push(view)
            const position     = Hud.commonDataModel.getFixedMiniviewPositionForIndex(miniviewIndex)
            view.x             = position.x
            view.y             = position.y
            view.width         = position.width
            view.height        = position.height
            view.parent        = rootItem
            view.miniviewIndex = miniviewIndex
        }

        console.assert(miniviewIndices.length === oldMiniviewIndices.length)
        console.assert(miniviewIndices.length === oldMiniviews.length)

        updateVisibilityOfTileHuds()
    }

    function updateVisibilityOfTileHuds() {
        for (const view of oldMiniviews) {
            // It seems like there is some conflict with the visible property, so we modify opacity. TODO wtf?
            view.opacity = rootItem.suppressShowingTileHuds ? 0.0 : 1.0
        }
    }

    function recycle(view, miniviewIndex) {
        view.parent        = null
        view.miniviewIndex = -1
        view.opacity       = 0.0
        instantiatedMiniviewHuds[miniviewIndex] = view
    }

    function take(miniviewIndex) {
        const maybeExistingView = instantiatedMiniviewHuds[miniviewIndex]
        if (maybeExistingView) {
            maybeExistingView.opacity = 1.0
            return maybeExistingView
        }
        const model = Hud.commonDataModel.getMiniviewModelForIndex(miniviewIndex)
        const view  = miniviewComponent.createObject(rootItem, {"povDataModel" : model})
        instantiatedMiniviewHuds[miniviewIndex] = view
        return view
    }

    Component {
        id: miniviewComponent

        MouseArea {
            id: miniviewItem
            hoverEnabled: isATileElement && !rootItem.suppressShowingTileHuds
            enabled: isATileElement && !rootItem.suppressShowingTileHuds
            property int miniviewIndex: -1
            // FIXME It gets stuck in "containsMouse" state upon click and chase mode switching
            property bool hackyContainsMouse

            onParentChanged: {
                if (!parent) {
                    hackyContainsMouse = false
                }
            }
            // TODO: Is it needed?
            onPositionChanged: hackyContainsMouse = true
            onEntered: {
                Hud.ui.playHoverSound()
                // Unset this flag in all other views
                for (const view of Object.values(rootItem.instantiatedMiniviewHuds)) {
                    if (view != miniviewItem) {
                        view.hackyContainsMouse = false
                    }
                }
                hackyContainsMouse = true
            }
            onExited: hackyContainsMouse = false

            // It gets specified via construction args
            property alias povDataModel: actualHudField.povDataModel
            property alias isATileElement: actualHudField.isATileElement

            onClicked: {
                Hud.ui.playForwardSound()
                Hud.ui.switchToPlayerNum(Hud.commonDataModel.getMiniviewPlayerNumForIndex(miniviewIndex))
            }

            InGameHud {
                id: actualHudField
                anchors.fill: parent
                layoutModel: Hud.commonDataModel.miniviewLayoutModel
                commonDataModel: Hud.commonDataModel
                isATileElement: miniviewItem.parent === rootItem
                clip: true
            }

            Rectangle {
                color: "transparent"
                anchors.centerIn: parent
                width: parent.width + 2.0 * border.width
                height: parent.height + 2.0 * border.width
                radius: Hud.elementRadius
                border.color: !isATileElement ? Qt.rgba(0.0, 0.0, 0.0, 0.7) :
                                ((miniviewIndex === Hud.commonDataModel.highlightedMiniviewIndex || miniviewItem.hackyContainsMouse) ?
                                    Material.accent : Qt.rgba(0.5, 0.5, 0.5, 1.0))
                border.width: Hud.miniviewBorderWidth
                Behavior on border.color { ColorAnimation { duration: 100 } }
            }
        }
    }

    Loader {
        active: Hud.ui.isShowingActionRequests
        anchors.top: parent.top
        anchors.bottom: parent.bottom
        anchors.right: parent.right
        anchors.margins: Hud.elementMargin
        width: 480
        sourceComponent: ActionRequestArea {}
    }
}