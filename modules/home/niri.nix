{...}: {
  home.file.".config/niri/config.kdl".text = ''
    // general settings
    prefer-no-csd
    screenshot-path "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png"

    spawn-at-startup "dunst"
    spawn-at-startup "swww-daemon"
    spawn-at-startup "waybar"
    spawn-at-startup "lxqt-policykit-agent"
    spawn-at-startup "wlsunset"
    environment {
      XDG_CURRENT_DESKTOP "niri"
      XDG_SESSION_DESKTOP "niri"
      XDG_SESSION_TYPE "wayland"
      GDK_BACKEND "wayland"
      GTK_CSD "0"
      CLUTTER_BACKEND "wayland"
      QT_QPA_PLATFORM "wayland"
      QT_WAYLAND_DISABLE_WINDOWDECORATION "1"
      QT_AUTO_SCREEN_SCALE_FACTOR "1"
      SDL_VIDEODRIVER "x11"
      MOZ_ENABLE_WAYLAND "1"
      NIXOS_OZONE_WL "1"
    }
    // input devices, yk
    input {
      keyboard {
        xkb {
          layout "us"
        }
      }

      touchpad {
        tap
        natural-scroll
      }
    }

    // output devices, monitor, yk
    output "DP-1" {
      mode "1920x1080@165.001"
      scale 1.0
      transform "normal"
      position x=1280 y=0
    }

    // general tiling layout
    layout {
      gaps 8
      center-focused-column "never"
      default-column-width { proportion 0.5; }
      focus-ring {
        width 2
        active-color "#8fa1b3"
        inactive-color "#4f5b66"
      }
    }

    binds {
      // shows a list of important hotkeys.
      Mod+Shift+Slash { show-hotkey-overlay; }

      // programs
      Mod+return { spawn "kitty"; }
      Mod+space { spawn "fuzzel"; }
      Super+Alt+L { spawn "swaylock"; }
      Mod+E { spawn "thunar"; }
      Mod+Shift+F { spawn "firefox"; };
      Ctrl+Alt+R { spawn "waybar"; } // debug keybind

      // screenshot utility
      Print { screenshot; }
      Ctrl+Print { screenshot-screen; }
      Alt+Print { screenshot-window; }

      // XF86 keys
      XF86AudioRaiseVolume allow-when-locked=true { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+"; }
      XF86AudioLowerVolume allow-when-locked=true { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-"; }
      XF86AudioMute        allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"; }
      XF86AudioMicMute     allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"; }

      // needs playerctl
      XF86AudioNext  { spawn "playerctl" "next"; }
      XF86AudioPause { spawn "playerctl" "play-pause"; }
      XF86AudioPlay  { spawn "playerctl" "play-pause"; }
      XF86AudioPrev  { spawn "playerctl" "previous"; }

      XF86MonBrightnessDown { spawn "brightnessctl" "s" "1%-"; }
      XF86MonBrightnessUp { spawn "brightnessctl" "s" "1%+"; }


      // quiting n killin
      Mod+Q { close-window; }
      Ctrl+Alt+Delete { quit; }

      // move focus
      Mod+Left  { focus-column-left; }
      Mod+Right { focus-column-right; }
      Mod+Up    { focus-workspace-up; }
      Mod+Down  { focus-workspace-down; }

      // move window and focus
      Mod+Shift+Left  { move-column-left; }
      Mod+Shift+Right { move-column-right; }

      // mouse movement
      Mod+WheelScrollUp          cooldown-ms=150 { focus-column-left; }
      Mod+WheelScrollDown        cooldown-ms=150 { focus-column-right; }
      Mod+Shift+WheelScrollUp    cooldown-ms=150 { focus-workspace-up; }
      Mod+Shift+WheelScrollDown  cooldown-ms=150 { focus-workspace-down; }

      // move specific workspace focus
      Mod+1      { focus-workspace 1; }
      Mod+2      { focus-workspace 2; }
      Mod+3      { focus-workspace 3; }
      Mod+4      { focus-workspace 4; }
      Mod+Ctrl+1 { move-column-to-workspace 1; }
      Mod+Ctrl+2 { move-column-to-workspace 2; }
      Mod+Ctrl+3 { move-column-to-workspace 3; }
      Mod+Ctrl+4 { move-column-to-workspace 4; }

      // move windows in and out of same column
      Mod+BracketLeft  { consume-or-expel-window-left; }
      Mod+BracketRight { consume-or-expel-window-right; }

      // window presets
      Mod+R       { switch-preset-column-width; }
      Mod+Shift+R { reset-window-height; }
      Mod+M       { maximize-column; }
      Mod+Shift+M { fullscreen-window; }
      Mod+C       { center-column; }

      // move windows percise
      Mod+Minus       { set-column-width "-10%"; }
      Mod+Equal       { set-column-width "+10%"; }
      Mod+Shift+Minus { set-window-height "-5%"; }
      Mod+Shift+Equal { set-window-height "+5%"; }
    }
  '';
}