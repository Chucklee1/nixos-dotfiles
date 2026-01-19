pragma Singleton
import QtQuick
import Quickshell

Singleton {
    // base16 nord palette
    property color base00: "#2e3440"
    property color base01: "#3b4252"
    property color base02: "#434c5e"
    property color base03: "#4c566a"
    property color base04: "#d8dee9"
    property color base05: "#e5e9f0"
    property color base06: "#eceff4"
    property color base07: "#8fbcbb"
    property color base08: "#bf616a"
    property color base09: "#d08770"
    property color base0A: "#ebcb8b"
    property color base0B: "#a3be8c"
    property color base0C: "#88c0d0"
    property color base0D: "#81a1c1"
    property color base0E: "#b48ead"
    property color base0F: "#5e81ac"

    // conventional names
    property color bg: base00
    property color bga: Qt.rgba(base00.r, base00.g, base00.b, 0.8) 
    property color hl: base0D 
    property color text: base06
    property color red: base08
    property color green: base0B
}
