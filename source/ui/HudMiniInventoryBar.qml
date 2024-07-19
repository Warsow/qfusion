import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import QtQuick.Layouts 1.12
import net.warsow 2.6

Item {
    id: root
    implicitWidth: 0.0
    implicitHeight: 0.0
    width: implicitWidth
    height: implicitHeight

    property var povDataModel
    property real miniviewScale
    property real widthLimit
    property int displayMode

    readonly property real actualScale: Math.max(10, 32 * miniviewScale) / 32.0

    readonly property real cardWidth: 45 * actualScale
    readonly property real cardHeight: 54 * actualScale
    readonly property real cardRadius: 2 + 7 * actualScale

    Component.onCompleted: applyLayout()
    onDisplayModeChanged: applyLayout()
    onMiniviewScaleChanged: applyLayout()
    onWidthLimitChanged: applyLayout()

    Repeater {
        id: repeater
        model: root.povDataModel.getInventoryModel()

        delegate: Item {
            id: delegateItem
            width: implicitWidth
            height: implicitHeight
            implicitWidth: visible ? cardWidth : 0
            implicitHeight: visible ? cardHeight : 0
            visible: model.displayed

            Behavior on implicitWidth {
                SmoothedAnimation { duration: 100 }
            }
            Behavior on implicitHeight {
                SmoothedAnimation { duration: 100 }
            }

            readonly property bool modelActive: model.active
            onModelActiveChanged: root.applyLayout()

            onImplicitWidthChanged: root.applyLayout()
            onImplicitHeightChanged: root.applyLayout()

            Component.onDestruction: {
                Hud.ui.ensureObjectDestruction(model)
                Hud.ui.ensureObjectDestruction(delegateItem)
            }

            Rectangle {
                id: frame
                anchors.centerIn: parent
                width: delegateItem.width
                height: cardHeight
                radius: cardRadius
                color: Hud.elementBackgroundColor
            }

            // Note: Check whether using states safe wrt memory leaks in this case
            // Actually we're reluctant to use states as there were state-related bugs (see HudMiniValueBar, HudMiniWeaponStatus)
            Rectangle {
                anchors.centerIn: parent
                width: delegateItem.visible ? (displayMode > Hud.DisplayMode.Compact ? delegateItem.width + 2 : Hud.tinyValueBarHeight + 1) : 0
                height: displayMode > Hud.DisplayMode.Compact ? cardHeight + 2 * border.width : (Hud.tinyValueBarHeight + 1)
                radius: displayMode > Hud.DisplayMode.Compact ? cardRadius : 1
                color: displayMode > Hud.DisplayMode.Compact ? "transparent" : Hud.ui.colorWithAlpha(model.color, model.hasWeapon ? 1.0 : 0.5)
                border.width: displayMode > Hud.DisplayMode.Compact ? Math.max(1, 4 * actualScale) : 1.5
                border.color: displayMode > Hud.DisplayMode.Compact ? Qt.lighter(model.color, 1.1) : model.color
                visible: displayMode === Hud.DisplayMode.Compact || active
            }

            Image {
                id: icon
                visible: delegateItem.width + 2 > icon.width && displayMode >= Hud.DisplayMode.Regular
                anchors.centerIn: parent
                width: 32 * actualScale
                height: 32 * actualScale
                source: visible ? (model.hasWeapon ? model.iconPath : (model.iconPath + "?grayscale=true")) : ""
                smooth: true
                mipmap: true
            }
        }
    }

    function applyLayout() {
        let columnSpacing
        let rowSpacing
        if (displayMode === Hud.DisplayMode.Compact) {
            columnSpacing = 0.0
            rowSpacing    = 1.0
        } else if (displayMode === Hud.DisplayMode.Extended) {
            columnSpacing = 8.0 * actualScale
            rowSpacing    = 8.0 * actualScale
        } else {
            columnSpacing = cardRadius
            rowSpacing    = cardRadius
        }

        let desiredWidth        = 0.0
        let displayedItemsCount = 0
        for (let i = 0; i < repeater.count; ++i) {
            const item = repeater.itemAt(i)
            if (!item) {
                // Wait for instantiation of all items
                root.implicitWidth  = 0.0
                root.implicitHeight = cardHeight
                return
            }
            // Check the current implicit height (account for anims of hiding items)
            if (item.implicitWidth > 0.0) {
                desiredWidth += cardWidth
                desiredWidth += columnSpacing
                displayedItemsCount++
            }
        }
        if (desiredWidth > 0.0) {
            desiredWidth -= columnSpacing
        }

        // The second condition protects assumptions on numFirstRowItems (but reaching the condition seems unlikely)
        if (desiredWidth <= root.widthLimit || displayedItemsCount <= 4) {
            // Apply a straightforward left-to-right linear layout
            let accumX = 0.0
            for (let i = 0; i < repeater.count; ++i) {
                const item = repeater.itemAt(i)
                if (item.implicitWidth > 0.0) {
                    item.x = accumX
                    item.y = 0.5 * (cardHeight - item.implicitHeight)
                    accumX += item.implicitWidth + columnSpacing
                }
            }
            root.implicitWidth  = accumX > 0.0 ? accumX - columnSpacing : 0.0
            root.implicitHeight = cardHeight
        } else {
            // Just chose two rows.
            // Iterate backwards during passes so best weapons are on top.
            // The bottom row should not have more items than the top one.
            let numFirstRowItems = Math.floor(displayedItemsCount / 2)
            if (2 * numFirstRowItems === displayedItemsCount) {
                numFirstRowItems--
            }

            let row1Width           = 0.0
            let row2Width           = 0.0
            let displayedItemsSoFar = 0
            for (let i = repeater.count - 1; i >= 0; --i) {
                const item = repeater.itemAt(i);
                if (item.implicitWidth > 0.0) {
                    if (displayedItemsSoFar < numFirstRowItems) {
                        displayedItemsSoFar++
                        row1Width += item.implicitWidth + columnSpacing
                    } else {
                        row2Width += item.implicitWidth + columnSpacing
                    }
                }
            }

            row1Width -= columnSpacing
            row2Width -= columnSpacing

            displayedItemsSoFar = 0
            let currRightmostX  = root.widthLimit - 0.5 * (root.widthLimit - row1Width)
            let accumBaseY      = 0
            for (let i = repeater.count - 1; i >= 0; --i) {
                const item = repeater.itemAt(i);
                if (item.implicitWidth > 0.0) {
                    if (displayedItemsSoFar < numFirstRowItems) {
                        displayedItemsSoFar++
                    } else if (displayedItemsSoFar === numFirstRowItems) {
                        // Mark the edge as detected
                        displayedItemsSoFar++;
                        // Switch to new row
                        currRightmostX = root.widthLimit - 0.5 * (root.widthLimit - row2Width)
                        accumBaseY     = cardHeight + rowSpacing
                    }
                    item.x = currRightmostX - item.implicitWidth
                    item.y = accumBaseY + 0.5 * (cardHeight - item.implicitHeight)
                    currRightmostX -= item.implicitWidth + columnSpacing
                }
            }

            root.implicitWidth  = root.widthLimit
            root.implicitHeight = 2 * cardHeight + rowSpacing
        }
    }
}