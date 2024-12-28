{pkgs, ...}: {
  programs.hyprland.enable = true;
  environment.systemPackages = [pkgs.hyprland-protocols];

  home-manager.sharedModules = [
    {
      wayland.windowManager.hyprland = {
        enable = true;
        plugins = [pkgs.hyprlandPlugins.hyprscroller];
        settings = {
          # general
          monitor = ", 1920x1080@165, auto, auto";
          xwayland.force_zero_scaling = true;
          input = {
            kb_layout = "${def.layout}";
            follow_mouse = 0;
            sensitivity = 0;
          };
          gestures = {
            workspace_swipe = false;
            workspace_swipe_forever = false;
          };
          misc = {
            disable_autoreload = true;
            force_default_wallpaper = 0;
            animate_mouse_windowdragging = false;
          };
          # env vars
          env = {
            NIXOS_OZONE_WL = 1;
            MOZ_ENABLE_WAYLAND = 1;

            XDG_CURRENT_DESKTOP = "Hyprland";
            XDG_SESSION_TYPE = "wayland";
            XDG_SESSION_DESKTOP = "Hyprland";

            GDK_BACKEND = "wayland,x11,*";
            CLUTTER_BACKEND = "wayland";
            QT_QPA_PLATFORM = "wayland;xcb";
            QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
            QT_AUTO_SCREEN_SCALE_FACTOR = 1;

            SDL_VIDEODRIVER = "x11";
            DISPLAY = ":0";
          };

          # window rules windowrule = <rule>,<info>
          #windowrulev2 = [];

          # startup
          exec-once = [
            "waybar"
            "nm-applet"
            "hyprsunset -t 5200"
            "swaybg -i $HOME/nixos-dotfiles/assets/wallpaper.png -m fill"
          ];
          # theming & layoutr
          general = {
            layout = "scroller";
            gaps_in = 2;
            gaps_out = 4;
            border_size = 2;
            allow_tearing = true;
            resize_on_border = true;
          };
          decoration = {
            rounding = 4;
            animations = {
              # bezier = NAME, X0, Y0, X1, Y1
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
                "windowsOut, 1, 4, ease-out-quad" # close`
                "windowsMove, 1, 2.9, spring"
                "fade, 0"
              ];
            };
          };
          # bindings: MODS, key, dispatcher, params
          # row: -- column: |
          "$mod" = "SUPER";
          bind = [
            # programs
            "$mod, return, exec, kitty"
            "$mod shift, return, exec, kitty -e tmux"
            "$mod, space, exec, fuzzel"

            "$mod, e, exec, thunar"
            "$mod shift, p, exec, wlogout"
            "$mod shift, l, exec, swaylock"
            # hyprland
            "$mod, v, togglefloating"
            ", ctrl+alt+del, exit"
            # scroller - overview windows
            "$mod shift, space, scroller:toggleoverview"
            # scroller - align windows
            "$mod, h, scroller:alignwindow, l"
            "$mod, l, scroller:alignwindow, r"
            # scroller - columns
            "$mod shift, j, scroller:admitwindow"
            "$mod shift, k, scroller:expelwindow"
            "$mod, m, scroller:fitsize, active" # toggle full window
            "$mod shift, m, fullscreen" # toggle full screen
            "$mod, p, scroller:pin" # toggle pin window
            # scroller - set window size
            "$mod, equal, scroller:cyclewidth, next"
            "$mod, minus, scroller:cyclewidth, prev"
            "$mod SHIFT, equal, scroller:cycleheight, next"
            "$mod SHIFT, minus, scroller:cycleheight, prev"
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
          ];
          bindm = [
            # mouse related
            "$mod, mouse:272, movewindow"
            "$mod, mouse:273, resizewindow"
            "$mod ALT, mouse:272, resizewindow"
          ];
          bindl = [
            # media keys
            ", XF86AudioPlay, exec, playerctl play-pause"
            ", XF86AudioPrev, exec, playerctl previous"
            ", XF86AudioNext, exec, playerctl next"
            ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
            ", XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
          ];
          bindle = [
            # why hyprland, must you have diffrent names for these
            ", XF86AudioRaiseVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 5%+"
            ", XF86AudioLowerVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 5%-"
            ", XF86MonBrightnessUp, exec, brightnessctl s 5%+"
            ", XF86MonBrightnessDown, exec, brightnessctl s 5%-"
          ];
        };
      };
    }
  ];
}
