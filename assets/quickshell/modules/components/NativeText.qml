import QtQuick
import qs.modules.config as CFG

Text {
    renderType: Text.NativeRendering
    color: CFG.Palette.text
    font.family: CFG.Font.family
    font.pixelSize: CFG.Font.size
}
