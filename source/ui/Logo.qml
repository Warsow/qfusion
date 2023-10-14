import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Controls.Material 2.12
import QtQuick.Shapes 1.12
import QtGraphicalEffects 1.12

Item {
    id: root

    property color fillColor: "white"

    property real implicitWidth: wswScale * (368.336 + 5.0 * spacing)
    property real implicitHeight: wswScale * (51.766)

    // TODO: This sucks, Path::scale is only available since QtQuick 2.14
    property real wswScale: 1.25

    readonly property real spacing: 2.5

    readonly property real aShift: 1.0 * spacing * wswScale
    readonly property real rShift: 2.0 * spacing * wswScale
    readonly property real sShift: 3.0 * spacing * wswScale
    readonly property real oShift: 4.0 * spacing * wswScale
    readonly property real wShift: 5.0 * spacing * wswScale

    readonly property real centeringShift: 0.5 * (root.width - root.implicitWidth)
    readonly property real animFrac: extraLength / centeringShift

    property real extraLength: 0.25 * (root.width - root.implicitWidth)
    /*
    SequentialAnimation {
        running: true
        loops: Animation.Infinite
        NumberAnimation {
            target: root
            property: "extraLength"
            from: 0.0
            to: root.centeringShift
            duration: 500
        }
        PauseAnimation {
            duration: 100
        }
        NumberAnimation {
            target: root
            property: "extraLength"
            from: 0.5 * root.centeringShift
            to: 0.0
            duration: 500
        }
    }*/

    readonly property int shapeLayerSamples: 2

    Item {
        anchors.centerIn: parent
        width: root.implicitWidth + 2 * root.centeringShift
        height: root.implicitHeight

        Shape {
            id: primaryShape
            //visible: false
            anchors.centerIn: parent
            width: root.implicitWidth + 2 * root.centeringShift
            height: root.implicitHeight
            //layer.enabled: true
            //layer.samples: root.shapeLayerSamples

            // W
            ShapePath {
                strokeWidth: 0.0
                fillColor: root.fillColor
                startX: root.centeringShift + wswScale * 19.598; startY: wswScale * 0.0
                PathLine { x: root.centeringShift + wswScale * 30.828; y: wswScale * 0.0 }
                PathLine { x: root.centeringShift + wswScale * 20.149; y: wswScale * 27.965 }
                PathLine { x: root.centeringShift + wswScale * 31.379; y: wswScale * 27.965 }
                PathLine { x: root.centeringShift + wswScale * 42.055; y: wswScale * 0.0 }
                PathLine { x: root.centeringShift + wswScale * 53.288; y: wswScale * 0.0 }
                PathLine { x: root.centeringShift + wswScale * 42.607; y: wswScale * 27.965 }
                PathLine { x: root.centeringShift + wswScale * 53.840; y: wswScale * 27.965 }
                PathLine { x: root.centeringShift + wswScale * 64.524; y: wswScale * 0.0 }
                PathLine { x: root.centeringShift + wswScale * 75.746; y: wswScale * 0.0 }
                PathLine { x: root.centeringShift + wswScale * 60.980; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 4.830; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 19.598; y: wswScale * 0.0 }
            }

            // A
            ShapePath {
                strokeWidth: 0.0
                fillColor: root.fillColor
                startX: root.centeringShift + wswScale * 77.738 + aShift; startY: wswScale * 0.0
                PathLine { x: root.centeringShift + wswScale * 105.973 + aShift; y: wswScale * 0.0 }
                PathLine { x: root.centeringShift + wswScale * 135.208 + aShift; y: wswScale * 0.0 }
                PathLine { x: root.centeringShift + wswScale * 119.473 + aShift; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 107.214 + aShift; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 112.893 + aShift; y: wswScale * 23.800 }
                PathLine { x: root.centeringShift + wswScale * 80.063 + aShift; y: wswScale * 23.800 }
                PathLine { x: root.centeringShift + wswScale * 74.382 + aShift; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 62.970 + aShift; y: wswScale * 38.675 }
            }

            // R
            ShapePath {
                strokeWidth: 0.0
                fillColor: root.fillColor
                startX: root.centeringShift + wswScale * 136.181 + rShift; startY: wswScale * 0.0
                PathLine { x: root.centeringShift + wswScale * 193.493 + rShift; y: wswScale * 0.0 }
                PathLine { x: root.centeringShift + wswScale * 185.882 + rShift; y: wswScale * 19.932 }
                PathLine { x: root.centeringShift + wswScale * 190.210 + rShift; y: wswScale * 19.932 }
                PathLine { x: root.centeringShift + wswScale * 183.053 + rShift; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 170.775 + rShift; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 176.455 + rShift; y: wswScale * 23.800 }
                PathLine { x: root.centeringShift + wswScale * 138.886 + rShift; y: wswScale * 23.800 }
                PathLine { x: root.centeringShift + wswScale * 133.206 + rShift; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 121.413 + rShift; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 136.181 + rShift; y: wswScale * 0.0 }
            }

            // §
            ShapePath {
                strokeWidth: 0.0
                fillColor: root.fillColor

                // Start from the leftmost top vertex
                startX: root.centeringShift + wswScale * 195.522 + sShift; startY: wswScale * 0.0
                // Two stretched vertices
                PathLine { x: root.centeringShift + wswScale * 368.336 + wShift + extraLength; y: wswScale * 0.0 }
                PathLine { x: root.centeringShift + wswScale * 364.704 + wShift + extraLength; y: wswScale * 9.520 }
                PathLine { x: root.centeringShift + wswScale * 204.171 + sShift; y: wswScale * 9.520 }
                PathLine { x: root.centeringShift + wswScale * 208.250 + sShift; y: wswScale * 13.090 }
                PathLine { x: root.centeringShift + wswScale * 235.920 + sShift; y: wswScale * 13.090 }
                PathLine { x: root.centeringShift + wswScale * 228.982 + sShift; y: wswScale * 31.256 }
                PathLine { x: root.centeringShift + wswScale * 237.442 + sShift; y: wswScale * 38.677 }
                PathLine { x: root.centeringShift + wswScale * 232.445 + sShift; y: wswScale * 51.765 }
                PathLine { x: root.centeringShift + wswScale * 0.0 - extraLength; y: wswScale * 51.765 }
                PathLine { x: root.centeringShift + wswScale * 3.636 - extraLength; y: wswScale * 42.245 }
                PathLine { x: root.centeringShift + wswScale * 223.590 + sShift; y: wswScale * 42.245 }
                PathLine { x: root.centeringShift + wswScale * 219.545 + sShift; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 193.038 + sShift; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 199.711 + sShift; y: wswScale * 21.187 }
                PathLine { x: root.centeringShift + wswScale * 190.528 + sShift; y: wswScale * 13.090 }
            }

            // O
            ShapePath {
                strokeWidth: 0.0
                fillColor: root.fillColor
                startX: root.centeringShift + wswScale * 249.019 + oShift; startY: wswScale * 13.090
                PathLine { x: root.centeringShift + wswScale * 304.977 + oShift; y: wswScale * 13.090 }
                PathLine { x: root.centeringShift + wswScale * 290.209 + oShift; y: wswScale * 51.765 }
                PathLine { x: root.centeringShift + wswScale * 234.251 + oShift; y: wswScale * 51.765 }
            }

            // the second W
            ShapePath {
                strokeWidth: 0.0
                fillColor: root.fillColor
                startX: root.centeringShift + wswScale * 307.162 + wShift; startY: wswScale * 13.091
                PathLine { x: root.centeringShift + wswScale * 318.393 + wShift; y: wswScale * 13.091 }
                PathLine { x: root.centeringShift + wswScale * 307.713 + wShift; y: wswScale * 41.056 }
                PathLine { x: root.centeringShift + wswScale * 318.944 + wShift; y: wswScale * 41.056 }
                PathLine { x: root.centeringShift + wswScale * 329.619 + wShift; y: wswScale * 13.091 }
                PathLine { x: root.centeringShift + wswScale * 340.852 + wShift; y: wswScale * 13.091 }
                PathLine { x: root.centeringShift + wswScale * 330.172 + wShift; y: wswScale * 41.056 }
                PathLine { x: root.centeringShift + wswScale * 341.404 + wShift; y: wswScale * 41.056 }
                PathLine { x: root.centeringShift + wswScale * 352.088 + wShift; y: wswScale * 13.091 }
                PathLine { x: root.centeringShift + wswScale * 363.311 + wShift; y: wswScale * 13.091 }
                PathLine { x: root.centeringShift + wswScale * 348.544 + wShift; y: wswScale * 51.766 }
                PathLine { x: root.centeringShift + wswScale * 292.394 + wShift; y: wswScale * 51.766 }
            }
        }

        Shape {
            id: holeShape
            visible: false
            anchors.centerIn: parent
            width: root.implicitWidth + 2 * root.centeringShift
            height: root.implicitHeight

            //layer.enabled: true
            //layer.samples: root.shapeLayerSamples

            // A-hole
            ShapePath {
                strokeWidth: 0.0
                fillColor: "white"
                startX: root.centeringShift + wswScale * 87.514; startY: wswScale * 9.522
                PathLine { x: root.centeringShift + wswScale * 120.347; y: wswScale * 9.520 }
                PathLine { x: root.centeringShift + wswScale * 118.985; y: wswScale * 13.090 }
                PathLine { x: root.centeringShift + wswScale * 86.151; y: wswScale * 13.090 }
            }

            // R-hole
            ShapePath {
                strokeWidth: 0.0
                fillColor: "white"
                startX: root.centeringShift + wswScale * 146.339; startY: wswScale * 9.520
                PathLine { x: root.centeringShift + wswScale * 179.582; y: wswScale * 9.520 }
                PathLine { x: root.centeringShift + wswScale * 177.215; y: wswScale * 13.090 }
                PathLine { x: root.centeringShift + wswScale * 144.974; y: wswScale * 13.090 }
            }

            // O-hole
            ShapePath {
                strokeWidth: 0.0
                fillColor: "white"
                startX: root.centeringShift + wswScale * 256.243 + oShift; startY: wswScale * 23.800
                PathLine { x: root.centeringShift + wswScale * 288.977 + oShift; y: wswScale * 23.800 }
                PathLine { x: root.centeringShift + wswScale * 283.977 + oShift; y: wswScale * 38.675 }
                PathLine { x: root.centeringShift + wswScale * 250.565 + oShift; y: wswScale * 38.675 }
            }

            // §-hole
            ShapePath {
                strokeWidth: 0.0
                fillColor: "white"
                startX: root.centeringShift + wswScale * 210.509 + sShift; startY: wswScale * 21.420
                PathLine { x: root.centeringShift + wswScale * 221.326 + sShift; y: wswScale * 21.420 }
                PathLine { x: root.centeringShift + wswScale * 218.144 + sShift; y: wswScale * 29.750 }
                PathLine { x: root.centeringShift + wswScale * 207.327 + sShift; y: wswScale * 29.750 }
            }
        }

        /*
        OpacityMask {
            anchors.fill: parent
            cached: true
            invert: true
            source: primaryShape
            maskSource: holeShape
        }*/
    }
}