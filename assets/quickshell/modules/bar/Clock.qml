import Quickshell
import QtQuick
import qs.modules.components

Rectangle {
    SystemClock {
        id: clock
        precision: SystemClock.Seconds
    }
    NativeText {
        id: timeBlock
        text: Qt.formatDateTime(clock.date, "yyyy-MM-dd | HH:mm:ss")
        anchors {
            verticalCenter: parent.verticalCenter
        }
        Component.onCompleted: {
            parent.width = timeBlock.contentWidth
        }
    }
}
