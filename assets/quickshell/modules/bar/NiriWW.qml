import QtQuick
import QtQuick.Layouts
import qs.modules.components

RowLayout {
    Repeater {
        model: niri.workspaces
        Rectangle {
            Layout.preferredWidth: 13
            Layout.fillHeight: true
            color: "transparent"

            NativeText {
                text: model.isFocused ? "" :
                      model.isActive  ? "" : ""
            }

            MouseArea {
                anchors.fill: parent
                onClicked: niri.focusWorkspaceById(model.id)
            }
        }
    }
    NativeText { 
        Layout.leftMargin: 8
        text: {
            const t =niri.focusedWindow?.title ?? ""
            const max = 50
            return t.length > max ? t.slice(0, max - 1) + "…" : t
        }
    }
}
