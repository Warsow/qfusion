// Copyright (C) 2017 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR LGPL-3.0-only OR GPL-2.0-only OR GPL-3.0-only

import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import net.warsow 2.6

ComboBox {
    id: root

    property bool autoFit: true
    property real minimumWidth
    property real menuItemWidth
    property bool wasVisible

    background.opacity: 0.67

    font.pointSize: UI.labelFontSize
    font.weight: UI.labelFontWeight
    font.letterSpacing: UI.labelLetterSpacing

    Material.theme: Material.Dark
    Material.accent: "orange"

    onHoveredChanged: {
        if (hovered) {
            UI.ui.playHoverSound()
        }
    }
    onPressedChanged: {
        if (pressed) {
            UI.ui.playSwitchSound()
        }
    }
    
    delegate: MenuItem {
        width: GridView.view.cellWidth
        height: 36
        text: modelData
        Material.foreground: root.currentIndex === index ? root.contentItem.Material.accent : root.contentItem.Material.foreground
        highlighted: root.highlightedIndex === index
        hoverEnabled: root.hoverEnabled
        Component.onCompleted: {
            contentItem.font.pointSize     = UI.labelFontSize
            contentItem.font.letterSpacing = UI.labelLetterSpacing
            contentItem.font.weight        = UI.labelFontWeight
        }
        onHoveredChanged: {
            if (hovered) {
                UI.ui.playHoverSound()
            }
        }
        onPressedChanged: {
            if (pressed) {
                UI.ui.playSwitchSound()
            }
        }
    }
    
    popup: Popup {
        y: 0
        height: Math.min(contentItem.implicitHeight + verticalPadding * 2, rootItem.height - topMargin - bottomMargin)
        transformOrigin: Item.Top
        topMargin: 12
        bottomMargin: 12
        verticalPadding: 8

        Material.theme: root.Material.theme
        Material.accent: root.Material.accent
        Material.primary: root.Material.primary

        contentItem: GridView {
            id: popupGridView
            clip: true
            cellHeight: 36
            implicitHeight: contentHeight
            model: root.delegateModel
            currentIndex: root.highlightedIndex
            highlightMoveDuration: 0

            ScrollIndicator.vertical: ScrollIndicator {}
        }

        background: Rectangle {
            radius: 5
            color: parent.Material.dialogColor
            opacity: 0.67

            layer.enabled: root.enabled
            layer.effect: ElevationEffect { elevation: 8 }
        }

        onVisibleChanged: repositionBlurRegion()
        onXChanged: repositionBlurRegion()
        onYChanged: repositionBlurRegion()
        onWidthChanged: repositionBlurRegion()
        onHeightChanged: repositionBlurRegion()

        onAboutToHide: {
            root.contentItem.visible = wasVisible
            root.indicator.visible   = wasVisible
            root.background.visible  = wasVisible

            rootItem.resetPopupMode()
        }

        function repositionBlurRegion() {
            if (popup.visible) {
                wasVisible = root.visible

                root.contentItem.visible = false
                root.indicator.visible   = false
                root.background.visible  = false

                const globalPos = parent.mapToGlobal(0, 0)
                const inset     = popup.background.radius

                rootItem.setOrUpdatePopupMode(Qt.rect(globalPos.x + inset, globalPos.y + inset,
                    background.width - 2 * inset, background.height - 2 * inset))
            }
        }
    }

    TextMetrics {
        id: textMetrics
        font: root.font
    }

    onModelChanged: updateSize()
    Component.onCompleted: updateSize()

    function updateSize() {
        let desiredWidth = root.minimumWidth
        // https://stackoverflow.com/a/45049993
        for (let i = 0; i < model.length; i++){
            textMetrics.text = model[i]
            desiredWidth     = Math.max(textMetrics.width, desiredWidth)
        }
        if (desiredWidth) {
            root.implicitWidth          = 72 + desiredWidth
            const menuItemWidth         = 56 + desiredWidth;
            // TODO: Use something more sophisticated
            const numColumns            = (model.length <= 5) ? 1 : 2
            popup.width                 = numColumns * (menuItemWidth + popup.padding) + 1
            popupGridView.cellWidth     = menuItemWidth
            popupGridView.implicitWidth = numColumns * menuItemWidth + 1
            popupGridView.forceLayout()
        }
    }
}