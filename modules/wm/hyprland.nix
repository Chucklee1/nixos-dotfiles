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
          hyprland.settings = {
            general = {
              gaps_in = 5;
              gaps_out = 5;
              border_size = 1;
              #"col.active_border" = "rgba(88888888)";
              #"col.inactive_border" = "rgba(00000088)";

              allow_tearing = true;
              resize_on_border = true;
            };
            decoration = {
              rounding = 16;
              blur = {
                enabled = true;
                brightness = 1.0;
                contrast = 1.0;
                noise = 0.01;

                vibrancy = 0.2;
                vibrancy_darkness = 0.5;

                passes = 4;
                size = 7;

                popups = true;
                popups_ignorealpha = 0.2;
              };

              shadow = {
                enabled = true;
                #color = "rgba(00000055)";
                ignore_window = true;
                offset = "0 15";
                range = 100;
                render_power = 2;
                scale = 0.97;
              };
            };

            input = {
              kb_layout = "us";

              # do not follow mouse on move
              follow_mouse = 0;
              accel_profile = "flat";
            };
            dwindle = {
              # keep floating dimentions while tiling
              pseudotile = true;
              preserve_split = true;
            };

            misc = {
              disable_autoreload = true;
              force_default_wallpaper = 0;
              animate_mouse_windowdragging = false;
              vrr = 1;
            };

            render.direct_scanout = true;

            gestures = {
              workspace_swipe = true;
              workspace_swipe_forever = true;
            };

            xwayland.force_zero_scaling = true;
            debug.disable_logs = false;

            # bind = MODS, key, dispatcher, params
            "$mod" = "SUPER";
            bind = [
              "$mod, return, exec, kitty -e tmux"
              ", Print, exec, g"
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
