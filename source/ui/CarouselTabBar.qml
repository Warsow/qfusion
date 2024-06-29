import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Controls.Material.impl 2.12
import net.warsow 2.6

PathView {
    id: root
    interactive: false
    implicitHeight: 48

    path: Path {
        startX: 0.5 * root.width
        startY: 0.5 * root.height + 4
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

    delegate: TabButton {
        id: button

        height: 48
        width: implicitWidth
        text: root.model[index]["text"]

        // Gets broken on first click, but still is helpful to highlight the current item initially
        checked: PathView.isCurrentItem

        font.weight: PathView.isCurrentItem ? Font.Black : Font.ExtraBold
        font.pointSize: PathView.isCurrentItem ? UI.labelFontSize + 1 : UI.labelFontSize
        Behavior on font.pointSize { SmoothedAnimation { duration: 250 } }
        font.letterSpacing: PathView.isCurrentItem ? 2.0 : 1.25
        Behavior on font.letterSpacing { SmoothedAnimation { duration: 250 } }

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