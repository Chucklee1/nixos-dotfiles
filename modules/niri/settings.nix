# -----------------------------------------------------------
# niri program settings
# -----------------------------------------------------------
{
  pkgs,
  lib,
  config,
  ...
}: {
  programs.niri = {
    settings = {
      # general
      prefer-no-csd = true;
      hotkey-overlay.skip-at-startup = true;
      screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";
      # enviorment variables
      environment = {
        DISPLAY = ":0";
        XDG_CURRENT_DESKTOP = "niri";
        XDG_SESSION_DESKTOP = "niri";
        XDG_SESSION_TYPE = "wayland";
        GDK_BACKEND = "wayland";
        GTK_CSD = "0";
        CLUTTER_BACKEND = "wayland";
        QT_QPA_PLATFORM = "wayland";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
        QT_AUTO_SCREEN_SCALE_FACTOR = "1";
        SDL_VIDEODRIVER = "wayland";
        MOZ_ENABLE_WAYLAND = "1";
        NIXOS_OZONE_WL = "1";
      };
      # inputs
      input.keyboard.xkb.layout = "us";
      input.mouse.accel-speed = 1.0;
      input.touchpad = {
        tap = true;
        dwt = true;
        natural-scroll = true;
        click-method = "clickfinger";
      };
      input.tablet.map-to-output = "eDP-1";
      input.touch.map-to-output = "eDP-1";
      # borders n gaps
      layout = {
        gaps = 8;
        border.width = 2;
        always-center-single-column = false;
      };
      # corner rounding
      window-rules = [
        {
          matches = [];
          draw-border-with-background = false;
          geometry-corner-radius = {
            top-left = 12.0;
            top-right = 12.0;
            bottom-left = 12.0;
            bottom-right = 12.0;
          };
          clip-to-geometry = true;
        }
      ];
      # monitors
      outputs."DP-1" = {
        enable = true;
        mode.width = 1920;
        mode.height = 1080;
        position.x = 0;
        position.y = 0;
        mode.refresh = 165.001;
      };

      # keybinds
      binds = with config.lib.niri.actions; {
        "Mod+Return".action.spawn = ["kitty" "--working-directory" "~/nixos-dotfiles"];
        "Mod+Space".action.spawn = "fuzzel";
        "Mod+E".action.spawn = "thunar";

        "XF86AudioRaiseVolume".action.spawn = ["wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+"];
        "XF86AudioLowerVolume".action.spawn = ["wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-"];
        "XF86AudioMute".action.spawn = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"];

        "XF86LaunchA".action.spawn = "firefox";
        "XF86LaunchB".action.spawn = "code";

        "Mod+Q".action = close-window;

        "Mod+Left".action = focus-column-left;
        "Mod+Right".action = focus-column-right;
        "Mod+Up".action = focus-workspace-up;
        "Mod+Down".action = focus-workspace-down;

        "Mod+Shift+Left".action = move-column-left;
        "Mod+Shift+Right".action = move-column-right;
        "Mod+Shift+Up".action = move-window-to-workspace-up;
        "Mod+Shift+Down".action = move-window-to-workspace-down;

        "Mod+Comma".action = consume-window-into-column;
        "Mod+Period".action = expel-window-from-column;

        "Mod+R".action = switch-preset-column-width;
        "Mod+M".action = maximize-column;
        "Mod+Shift+M".action = fullscreen-window;

        "Mod+Minus".action = set-column-width "-10%";
        "Mod+Plus".action = set-column-width "+10%";
        "Mod+Shift+Minus".action = set-window-height "-10%";
        "Mod+Shift+Plus".action = set-window-height "+10%";

        "Mod+Shift+E".action = quit;
        "Mod+Shift+P".action.spawn = "wlogout";
      };
    };
  };
}
