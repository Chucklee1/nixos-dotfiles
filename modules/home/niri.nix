{
  pkgs,
  lib,
  config,
  ...
}: {
  programs.niri.settings = {
    # general
    prefer-no-csd = true;
    hotkey-overlay.skip-at-startup = false;
    screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";
    # startup
    spawn-at-startup = let
      # Helper function for session-based wrappers
      get-wayland-display = "systemctl --user show-environment | awk -F 'WAYLAND_DISPLAY=' '{print $2}' | awk NF";
      wrapper = name: op:
        pkgs.writeScript "${name}" ''
          if [ "$(${get-wayland-display})" ${op} "$WAYLAND_DISPLAY" ]; then
            exec "$@"
          fi
        '';

      only-on-session = wrapper "only-on-session" "=";
      only-without-session = wrapper "only-without-session" "!=";
    in [
      # Waybar
      {
        command = ["${lib.getExe pkgs.waybar}"];
      }

      # LXQt PolicyKit Agent
      {
        command = ["${lib.getExe pkgs.lxqt.lxqt-policykit}"];
      }

      # Dunst
      {
        command = ["${lib.getExe pkgs.dunst}"];
      }

      # swww-daemon
      {
        command = ["${lib.getExe pkgs.swww}" "swww-daemon"];
      }
      # wlsunset
      {
        command = [
          "${lib.getExe pkgs.wlsunset}"
          "-l"
          "59.0"
          "-L"
          "11.0"
          "-t"
          "6500"
          "-T"
          "3500"
        ];
      }
    ];
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

    # input.warp-mouse-to-focus = true;

    layout = {
      gaps = 8;
      border.width = 2;
      border.active.color = "#7AA2F7";
      always-center-single-column = false;
    };
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

      "Mod+Q".action = close-window;

      "Mod+Left".action = focus-column-left;
      "Mod+Right".action = focus-column-right;
      "Mod+Up".action = focus-workspace-up;
      "Mod+Down".action = focus-workspace-down;

      "Mod+Shift+Left".action = move-column-left;
      "Mod+Shift+Right".action = move-column-right;
      "Mod+Shift+Up".action = move-window-to-workspace-up;
      "Mod+Shift+Down".action = move-window-to-workspace-down;
    };
  };
}
