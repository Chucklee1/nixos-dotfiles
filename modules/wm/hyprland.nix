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
            windowrulev2 = "nofocus,class:^$,title:^$,xwayland:1,floating:0,fullscreen:0,pinned:0";

            # startup
            exec-once = [
              "nm-applet"
              "lxqt-policykit-agent"
              "wlsunset -T 5200"
              "swww-daemon && swww img $HOME/nixos-dotfiles/assets/wallpaper.PNG"
            ];

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

            # theming related
            general = {
              layout = "scroller";
              gaps_in = 4;
              gaps_out = 4;
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
              animations = {
                #bezier = NAME, X0, Y0, X1, Y1
                bezier = [
                  "ease-out-quad, 0.76, 0, 0.24, 1"
                  "ease-out-cubic, 0.33, 1, 0.68, 1"
                  "ease-out-expo, 0.16, 1, 0.3, 1"
                  "spring, 0.25, 0.75, 0.50, 1.0"
                ];
                # animation: NAME, ONOFF, SPEED, CURVE [,STYLE]
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
              # programs
              "$mod, return, exec, kitty -e tmux"
              "$mod, space, exec, fuzzel"
              "$mod shift, p, exec, wlogout"
              "$mod shift, l, exec, swaylock"
              # hyprland
              "$mod, f, togglefloating"
              "$mod, m, scroller:fitsize, active"
              "$mod shift, m, fullscreen"
              "$mod shift, space, scroller:toggleoverview"
              ", ctrl+alt+del, exit"

              # windows
              "$mod, Q, killactive"
              "$mod, left, movefocus, l"
              "$mod, right, movefocus, r"
              "$mod, up, movefocus, u"
              "$mod, down, movefocus, d"
              "$mod shift, left, movewindow, l"
              "$mod shift, right, movewindow, r"
              "$mod shift, up, movewindow, u"
              "$mod shift, down, movewindow, d"
              # workspaces
              "$mod ctrl, up, workspace, -1"
              "$mod ctrl, down, workspace, +1"
              "$mod shift ctrl, up, movetoworkspace, -1"
              "$mod shift ctrl, down, movetoworkspace, +1"
            ];
            bindm = [
              "$mod, mouse:272, movewindow"
              "$mod, mouse:273, resizewindow"
              "$mod ALT, mouse:272, resizewindow"
            ];
            bindl = [
              ", XF86AudioPlay, exec, playerctl play-pause"
              ", XF86AudioPrev, exec, playerctl previous"
              ", XF86AudioNext, exec, playerctl next"
              ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
              ", XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
            ];

            bindle = [
              ", XF86AudioRaiseVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 5%+"
              ", XF86AudioLowerVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 5%-"
              ", XF86MonBrightnessUp, exec, brightnessctl s 5%+"
              ", XF86MonBrightnessDown, exec, brightnessctl s 5%-"
            ];
          };
        };
      }
    ];
  };
}
