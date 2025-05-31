import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

Item {
    id: root

    property var commonDataModel
    property int paneNumber
    property var miniviewAllocator

    property var oldMiniviews: []
    property var oldMiniviewIndices: []

    width: implicitWidth
    height: implicitHeight

    Component.onCompleted: {
        console.assert(miniviewAllocator)
        implicitWidth  = 0
        implicitHeight = 0
        applyMiniviewLayoutPass1()
        applyMiniviewLayoutPass2()
    }

    // Do nothing on destruction, we don't destroy miview huds except when shutting down

    Connections {
        target: Hud.ui
        onDisplayedHudItemsRetrievalRequested: {
            for (const miniview of oldMiniviews) {
                Hud.ui.supplyDisplayedHudItemAndMargin(miniview, 4.0)
            }
        }
        onHudMiniviewPanesRetrievalRequested: {
            if (parent) {
                console.assert(parent instanceof HudLayoutItem)
                // Don't let occlusion affect the reported number of panes.
                // Otherwise it's possible to indroduce flicker loops
                // due to moving all views to another pane which becomes occluded next frame.
                if (parent.shouldBeVisibleIfNotOccluded()) {
                    Hud.ui.supplyHudMiniviewPane(paneNumber)
                }
            }
        }
        onHudControlledMiniviewItemsRetrievalRequested: {
            for (let i = 0; i < oldMiniviews.length; ++i) {
                if (oldMiniviews[i].parent && oldMiniviews[i].visible) {
                    Hud.ui.supplyHudControlledMiniviewItemAndModelIndex(oldMiniviews[i], oldMiniviewIndices[i])
                }
            }
        }
    }

    Connections {
        target: commonDataModel
        onMiniviewLayoutChangedPass1: applyMiniviewLayoutPass1()
        onMiniviewLayoutChangedPass2: applyMiniviewLayoutPass2()
    }

    function delegatedUpdateVisibility() {
        if (parent) {
            console.assert(parent instanceof HudLayoutItem)
            if (parent.shouldBeVisibleIfNotOccluded()) {
                for (const miniview of oldMiniviews) {
                    miniview.visible = !Hud.ui.isHudItemOccluded(miniview)
                }
            } else {
                for (const miniview of oldMiniviews) {
                    miniview.visible = false
                }
            }
        }
    }

    function applyMiniviewLayoutPass1() {
        const miniviewIndices = commonDataModel.getMiniviewIndicesForPane(paneNumber)
        for (let i = 0; i < oldMiniviews.length;) {
            const oldMiniviewIndex = oldMiniviewIndices[i]
            if (!miniviewIndices.includes(oldMiniviewIndex)) {
                const view = oldMiniviews[i]
                oldMiniviewIndices.splice(i, 1)
                oldMiniviews.splice(i, 1)
                miniviewAllocator.recycle(view, oldMiniviewIndex)
            } else {
                ++i
            }
        }
    }

    function applyMiniviewLayoutPass2() {
        const miniviewIndices = commonDataModel.getMiniviewIndicesForPane(paneNumber)
        for (const miniviewIndex of miniviewIndices) {
            if (!oldMiniviewIndices.includes(miniviewIndex)) {
                const view         = miniviewAllocator.take(miniviewIndex)
                view.miniviewIndex = miniviewIndex
                oldMiniviewIndices.push(miniviewIndex)
                oldMiniviews.push(view)
            }
        }

        console.assert(miniviewIndices.length === oldMiniviewIndices.length)
        console.assert(miniviewIndices.length === oldMiniviews.length)

        const preferredNumRows    = commonDataModel.getPreferredNumRowsForMiniviewPane(paneNumber)
        const preferredNumColumns = commonDataModel.getPreferredNumColumnsForMiniviewPane(paneNumber)

        let allowedNumRows    = commonDataModel.getAllowedNumRowsForMiniviewPane(paneNumber)
        let allowedNumColumns = commonDataModel.getAllowedNumColumnsForMiniviewPane(paneNumber)

        let chosenNumRows    = allowedNumRows
        let chosenNumColumns = allowedNumColumns
        // If preferred values are feasible
        if (preferredNumRows <= allowedNumRows && preferredNumColumns <= allowedNumColumns) {
            // If we can lay out all items using preferred values
            if (oldMiniviews.length <= preferredNumRows * preferredNumColumns) {
                chosenNumRows    = preferredNumRows
                chosenNumColumns = preferredNumColumns
            }
        }

        console.assert(chosenNumRows > 0 && chosenNumColumns > 0)

        const chosenItemWidth  = Hud.miniviewItemWidth - 2 * Hud.miniviewBorderWidth
        const chosenItemHeight = Hud.miniviewItemHeight - 2 * Hud.miniviewBorderWidth
        const spacing          = Hud.elementMargin + Hud.miniviewBorderWidth

        let accumHeight   = Hud.miniviewBorderWidth
        let maxAccumWidth = 0
        let currViewIndex = 0
        for (let rowNum = 0; rowNum < chosenNumRows && currViewIndex < oldMiniviews.length; ++rowNum) {
            let accumWidth = Hud.miniviewBorderWidth
            for (let columnNum = 0; columnNum < chosenNumColumns && currViewIndex < oldMiniviews.length; ++columnNum) {
                const view  = oldMiniviews[currViewIndex]
                currViewIndex++
                view.x      = accumWidth
                view.y      = accumHeight
                view.width  = chosenItemWidth
                view.height = chosenItemHeight
                view.parent = root
                accumWidth += chosenItemWidth
                if (columnNum + 1 < chosenNumColumns) {
                    accumWidth += spacing
                }
            }
            maxAccumWidth = Math.max(accumWidth, maxAccumWidth)
            accumHeight += chosenItemHeight
            if (rowNum + 1 < chosenNumRows && currViewIndex < oldMiniviews.length) {
                accumHeight += spacing
            }
        }

        root.implicitWidth  = maxAccumWidth + Hud.miniviewBorderWidth
        root.implicitHeight = accumHeight + Hud.miniviewBorderWidth

        delegatedUpdateVisibility()
    }
}