import QtQuick
import Quickshell.Services.Pipewire
import qs.modules.components
import qs.modules.config as Cfg

Item {
    id: root
    implicitWidth: 60
    implicitHeight: 24

    // track the default sink so audio fields become valid
    PwObjectTracker {
        objects: [Pipewire.defaultAudioSink]
    }

    NativeText {
        anchors.centerIn: parent
        text: {
            const sink = Pipewire.defaultAudioSink
            const vol = Math.round(sink.audio.volume * 100)
            if (!sink || !sink.audio || sink.audio.muted) return ""
            return vol + "%"
        }
    }

    NativeText {
        anchors.centerIn: parent
        leftPadding: 60
        text: {
            const sink = Pipewire.defaultAudioSink
            if (!sink || !sink.audio) return ""

            const vol = Math.round(sink.audio.volume * 100)
            const icon = sink.audio.muted
                       ? ""
                       : vol === 0 ? ""
                       : vol < 50  ? ""
                       : ""
            return icon
        }

        color: {
            const sink = Pipewire.defaultAudioSink
            if (!sink || !sink.audio) return Cfg.Palette.bg
            if (sink.audio.muted) return Cfg.Palette.red
            return Cfg.Palette.green
        }
    }
}
