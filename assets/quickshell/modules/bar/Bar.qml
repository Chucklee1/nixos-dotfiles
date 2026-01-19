import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.modules.config as Cfg

PanelWindow {
    anchors {
        top: true
        left: true
        right: true
    }
    implicitHeight: 24
    color: Cfg.Palette.bga
    // left
    RowLayout {
        spacing: 4
        anchors {
            verticalCenter: parent.verticalCenter
            left: parent.left
            leftMargin: 5
        }
        Loader { active: true; sourceComponent: NiriWW {} }
    }
    // center
    RowLayout {
        anchors {
            horizontalCenter: parent.horizontalCenter
            verticalCenter: parent.verticalCenter
        }
        Loader { active: true; sourceComponent: Clock {}  }
    }
    // right
    RowLayout {
        anchors {
            verticalCenter: parent.verticalCenter
            right: parent.right
            rightMargin: 15
        }
        Loader { active: true; sourceComponent: Pipewire {}  }
    }
}
