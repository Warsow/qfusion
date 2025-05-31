import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import net.warsow 2.6

// Make Loader the top element to reduce layout complexity of in-game HUDs
Loader {
    // Translates natively supplied (numeric) flags to an AnchorLine of another item.
    // Assumes that flags consist of two parts each of that has a single bit set.
    // This means that a HUD item position is described by 2 of 6 supported anchors
    // (leaving the unused anchors.baseline out of scope).
    function getQmlAnchorOfItem(selfAnchors, otherAnchors, anchorBit, anchorItem) {
        // If this anchorBit is specfied for self anchors
        if (selfAnchors & anchorBit) {
            const horizontalMask = HudLayoutModel.Left | HudLayoutModel.HCenter | HudLayoutModel.Right
            // If this anchor bit describes a horizontal rule
            if (anchorBit & horizontalMask) {
                // Check what horizontal anchor of other item is specified
                switch (otherAnchors & horizontalMask) {
                    case HudLayoutModel.Left: return anchorItem.left
                    case HudLayoutModel.HCenter: return anchorItem.horizontalCenter
                    case HudLayoutModel.Right: return anchorItem.right
                }
                console.log("unreachable")
            }
            // If this anchor bit describes a vertial rule
            const verticalMask = HudLayoutModel.Top | HudLayoutModel.VCenter | HudLayoutModel.Bottom
            if (anchorBit & verticalMask) {
                // Check what vertical anchor of other item is specified
                switch (otherAnchors & verticalMask) {
                    case HudLayoutModel.Top: return anchorItem.top
                    case HudLayoutModel.VCenter: return anchorItem.verticalCenter
                    case HudLayoutModel.Bottom: return anchorItem.bottom
                }
                console.log("unreachable")
            }
        }
        return undefined
    }

    function hasAnchorMarginWithItem(selfAnchors, otherAnchors, anchorBit) {
        // Don't try to get magins for center anchors
        console.assert(!(anchorBit & (HudLayoutModel.HCenter | HudLayoutModel.VCenter)))
        if (selfAnchors & anchorBit) {
            if (anchorBit === HudLayoutModel.Left) {
                if (otherAnchors & HudLayoutModel.Right) {
                    return true
                }
            } else if (anchorBit === HudLayoutModel.Right) {
                if (otherAnchors & HudLayoutModel.Left) {
                    return true
                }
            } else if (anchorBit === HudLayoutModel.Top) {
                if (otherAnchors & HudLayoutModel.Bottom) {
                    return true
                }
            } else if (anchorBit === HudLayoutModel.Bottom) {
                if (otherAnchors & HudLayoutModel.Top) {
                    return true
                }
            }
        }
        return false
    }

    function hasAnchorMarginWithField(selfAnchors, otherAnchors, anchorBit) {
        // Don't try to get magins for center anchors
        console.assert(!(anchorBit & (HudLayoutModel.HCenter | HudLayoutModel.VCenter)))
        if (selfAnchors & anchorBit) {
            if ((selfAnchors & anchorBit) === (otherAnchors & anchorBit)) {
                return true
            }
        }
        return false
    }
}