import Quickshell
import QtQuick
import Niri 0.1
import "modules/bar/"

ShellRoot {
    Niri {
        id: niri
        Component.onCompleted: connect()
    }

    LazyLoader{ active: true; component: Bar{} }
}


