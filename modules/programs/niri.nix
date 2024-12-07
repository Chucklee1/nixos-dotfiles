{
  inputs,
  pkgs,
  lib,
  config,
  ...
}: {
  nixpkgs.overlays = [inputs.niri.overlays.niri];
  home-manager.shared-modules = [
    {
      programs.niri = {
        enable = true;
        package = pkgs.niri-unstable;
        settings = {
          prefer-no-csd = true;
          hotkey-overlay.skip-at-startup = true;
          screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";

          environment = {
            NIXOS_OZONE_WL = "1";
            XDG_SESSION_TYPE = "wayland";
            XDG_CURRENT_DESKTOP = "niri";
            XDG_SESSION_DESKTOP = "niri";
            DISPLAY = ":0";
            GDK_BACKEND = "wayland";
            GTK_CSD = "0";
            CLUTTER_BACKEND = "wayland";
            QT_QPA_PLATFORM = "wayland;xcb";
            QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
            QT_AUTO_SCREEN_SCALE_FACTOR = "1";
            SDL_VIDEODRIVER = "x11";
            MOZ_ENABLE_WAYLAND = "1";
          };

          spawn-at-startup = [
            {command = ["lxqt-policykit"];}
            {command = ["dunst"];}
            {command = ["xwayland-satellite"];}
            {command = ["nm-applet"];}
            {command = ["wlsunset" "-t" "5000" "-T" "6500"];}
            {command = ["swww-daemon"];}
            {command = ["swww" "img" "/home/goat/nixos-dotfiles/Pictures/mono-forest.PNG"];}
            {command = ["waybar"];}
          ];

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

          outputs."DP-1" = {
            enable = true;
            mode.width = 1920;
            mode.height = 1080;
            position.x = 0;
            position.y = 0;
            mode.refresh = 165.001;
          };

          binds = let
            sh = "sh" "-c";
            msg = "niri" "msg" "action";
          in {
            # programs
            "Mod+Return".action.spawn = sh ''kitty --working-directory ~/nixos-dotfiles'';
            "Mod+Space".action = "fuzzel";
            "Super+Alt+L".action = "swaylock";
            "Super+Alt+P".action = "wlogout";

            # media keys
            "XF86AudioRaiseVolume".action.spawn = sh ''wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+'';
            "XF86AudioLowerVolume".action.spawn = sh ''wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-'';
            "XF86AudioMute".action.spawn = sh ''wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle'';
            "XF86AudioMicMute".action.spawn = sh ''wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle'';
            "XF86MonBrightnessUp ".action.spawn = sh ''brightnessctl --device=amdgpu_bl1 s 5%+'';
            "XF86MonBrightNessDown".action.spawn = sh ''brightnessctl --device=amdgpu_bl1 s 5%-'';

            # screenshot
            "Print".action = msg "screenshot";
            "Ctrl+Print".action = msg "screenshot-screen";
            "Alt+Print".action = msg "screenshot-window";

            # window actions
            "Mod+Q".action = msg "close-window";
            "Ctrl+Alt+Delete".action = msg "quit";

            "Mod+Left".action = msg "focus-column-left";
            "Mod+Right".action = msg "focus-column-right";
            "Mod+Up".action = msg "focus-workspace-up";
            "Mod+Down".action = msg "focus-workspace-down";

            "Mod+Shift+Left".action = msg "move-column-left";
            "Mod+Shift+Right".action = msg "move-column-right";
            "Mod+Shift+Up".action = msg "move-window-to-workspace-up";
            "Mod+Shift+Down".action = msg "move-window-to-workspace-down";

            "Mod+Comma".action = msg "consume-window-into-column";
            "Mod+Period".action = msg "expel-window-from-column";

            "Mod+R".action = msg "switch-preset-column-width";
            "Mod+M".action = msg "maximize-column";
            "Mod+Shift+M".action = msg "fullscreen-window";

            "Mod+Minus".action = msg "set-column-width" "-10%";
            "Mod+Plus".action = msg "set-column-width" "+10%";
            "Mod+Shift+Minus".action = msg "set-window-height" "-1%";
            "Mod+Shift+Plus".action = msg "set-window-height" "+1%";
          };
        };
      };
    }
  ];
}
