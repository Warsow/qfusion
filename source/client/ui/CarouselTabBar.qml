import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import net.warsow 2.6

PathView {
    id: root
    interactive: false
    implicitHeight: UI.tabHeight

    path: Path {
        startX: 0.5 * root.width
        startY: 0.5 * root.height
        PathLine { relativeX: +0.5 * root.width; relativeY: 0 }
        PathMove { relativeX: -1.0 * root.width; relativeY: 0 }
        PathLine { relativeX: +0.5 * root.width; relativeY: 0 }
    }

    Rectangle {
        height: 4
        radius: 1
        z: 2
        width: Math.max(root.currentItem.width, 128)
        y: root.currentItem.height
        x: 0.5 * (root.width - width)
        Behavior on width { SmoothedAnimation { duration: 125 } }
        Behavior on x { SmoothedAnimation { duration: 125 } }
        color: UI.ui.colorWithAlpha(Material.accentColor, root.enabled ? 1.0 : 0.7)
    }

    delegate: UITabButton {
        id: button

        text: root.model[index]["text"]

        // Gets broken on first click, but still is helpful to highlight the current item initially
        checked: PathView.isCurrentItem

        Component.onCompleted: {
            // Hacks to disable darkening of tab buttons under the "accept/decline" settings overlay
            contentItem.color = Qt.binding(() => {
                const color = (button.down || button.checked) ? root.Material.accentColor : root.Material.foreground
                return UI.ui.colorWithAlpha(color, root.enabled ? 1.0 : 0.7)
            })
        }

        onClicked: {
            if (!PathView.isCurrentItem) {
                UI.ui.playForwardSound()
                root.currentIndex = index
            }
        }
    }
}