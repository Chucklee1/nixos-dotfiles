{
  config,
  lib,
  pkgs,
  defaults,
  ...
}: {
  options.hyprland.enable = lib.mkEnableOption "enable hyprland window manager";

  config = lib.mkIf config.hyprland.enable {
    programs.hyprland.enable = true;

    home-manager.sharedModules = [
      {
        wayland.windowManager.hyprland = {
          enable = true;
          plugins = [pkgs.hyprlandPlugins.hyprscroller];
          settings = {
            # general
            monitor = ", 1920x1080@165, auto, auto";
            input = {
              kb_layout = "us";
              follow_mouse = 1;
              sensitivity = 0;
            };

            misc = {
              disable_autoreload = true;
              force_default_wallpaper = 0;
              animate_mouse_windowdragging = false;
            };

            gestures = {
              workspace_swipe = true;
              workspace_swipe_forever = true;
            };

            xwayland.force_zero_scaling = true;
            render.direct_scanout = true;
            debug.disable_logs = false;

            # theming related
            general = {
              layout = "scroller";
              gaps_in = 5;
              gaps_out = 5;
              border_size = 2;
              allow_tearing = true;
              resize_on_border = true;
            };
            decoration = {
              rounding = 8;
              blur = {
                enabled = true;
                brightness = 1.0;
                contrast = 1.0;
                noise = 0.01;
                vibrancy = 0.2;
                vibrancy_darkness = 0.5;
                passes = 5;
                size = 3;
                popups = true;
                popups_ignorealpha = 0.2;
              };
              shadow = {
                enabled = true;
                ignore_window = true;
                offset = "0 15";
                range = 100;
                render_power = 2;
                scale = 0.97;
              };
              # animation: NAME, ONOFF, SPEED, CURVE [,STYLE]
              animations = {
                #bezier = NAME, X0, Y0, X1, Y1
                bezier = [
                  "ease-out-quad, 0.76, 0, 0.24, 1"
                  "ease-out-cubic, 0.33, 1, 0.68, 1"
                  "ease-out-expo, 0.16, 1, 0.3, 1"
                  "spring, 0.25, 0.75, 0.50, 1.0"
                ];
                animation = [
                  "workspaces, 1, 1.5, spring"
                  "windows, 1, 1.5, spring"
                  "windowsIn, 1, 4, ease-out-expo" # open
                  "windowsOut, 1, 4, ease-out-quad" # close
                  "windowsMove, 1, 2.9, spring"
                  "fade, 0"
                ];
              };
            };

            # bindings: MODS, key, dispatcher, params
            "$mod" = "SUPER";
            bind = [
              "$mod, return, exec, kitty -e tmux"
              "$mod, space, exec, fuzzel"

              "$mod, Q, killactive"
              "$mod, M, fullscreen,"
              ", ctrl+alt+del, exit"

              "$mod, left, movefocus, l"
              "$mod, right, movefocus, r"
              "$mod, up, movefocus, u"
              "$mod, down, movefocus, d"
            ];
            bindm = [
              "$mod, mouse:272, movewindow"
              "$mod, mouse:273, resizewindow"
              "$mod ALT, mouse:272, resizewindow"
            ];
            bindl = [
              # media controls
              ", XF86AudioPlay, exec, playerctl play-pause"
              ", XF86AudioPrev, exec, playerctl previous"
              ", XF86AudioNext, exec, playerctl next"

              # volume
              ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
              ", XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
            ];

            bindle = [
              # volume
              ", XF86AudioRaiseVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 5%+"
              ", XF86AudioLowerVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 5%-"
              # backlight
              ", XF86MonBrightnessUp, exec, brightnessctl s 5%+"
              ", XF86MonBrightnessDown, exec, brightnessctl s 5%-"
            ];
          };
        };
      }
    ];
  };
}
