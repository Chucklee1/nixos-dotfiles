{
  lib,
  config,
  pkgs,
  inputs,
  ...
}: {
  options = {
    niri.enable = lib.mkEnableOption "enable niri window manager";
  };
  config = lib.mkIf config.niri.enable {
    nixpkgs.overlays = [inputs.niri.overlays.niri];
    programs.niri = {
      enable = true;
      package = pkgs.niri-unstable;
    };
    home-manager.sharedModules = [
      {
        programs.fuzzel.enable = true;
        programs.wlogout.enable = true;

        programs.niri.settings = {
          prefer-no-csd = true;
          hotkey-overlay.skip-at-startup = true;
          screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";
          # setting env vars in niri settings ensures variables only start when niri starts
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
            {command = ["${lib.getExe pkgs.dunst}"];}
            {command = ["${lib.getExe pkgs.waybar}"];}
            {command = ["${lib.getExe pkgs.networkmanagerapplet}"];}
            {command = ["${lib.getExe pkgs.lxqt.lxqt-policykit}"];}
            {command = ["${lib.getExe pkgs.xdg-desktop-portal}"];}
            {command = ["${lib.getExe pkgs.xdg-desktop-portal-gtk}"];}
            {command = ["${lib.getExe pkgs.wlsunset}" "sh" "-c" "-T 5500"];}
          ];

          binds = let
            spawn = command: {action.spawn = ["sh" "-c" ''${command}''];};
            action = command: {action.spawn = ["sh" "-c" ''niri msg action ${command}''];};
          in {
            # programs
            "Mod+Return" = spawn "kitty --working-directory ~/nixos-dotfiles";
            "Mod+Space" = spawn "fuzzel";
            "Super+Shift+L" = spawn "swaylock";
            "Super+Shift+P" = spawn "wlogout";

            # media keys
            "XF86AudioRaiseVolume" = spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+";
            "XF86AudioLowerVolume" = spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-";
            "XF86AudioMute" = spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
            "XF86AudioMicMute" = spawn "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle";
            "XF86MonBrightnessUp" = spawn "${lib.getExe pkgs.brightnessctl}l --device=amdgpu_bl1 s 5%+";
            "XF86MonBrightNessDown" = spawn "${lib.getExe pkgs.brightnessctl} --device=amdgpu_bl1 s 5%-";

            # screenshot
            "Print" = spawn "screenshot";
            "Ctrl+Print" = action "screenshot-screen";
            "Alt+Print" = action "screenshot-window";

            # window actions
            "Mod+Q" = action "close-window";
            "Ctrl+Alt+Delete" = action "quit";

            "Mod+Left" = action "focus-column-left";
            "Mod+Right" = action "focus-column-right";
            "Mod+Up" = action "focus-workspace-up";
            "Mod+Down" = action "focus-workspace-down";

            "Mod+Shift+Left" = action "move-column-left";
            "Mod+Shift+Right" = action "move-column-right";
            "Mod+Shift+Up" = action "move-window-to-workspace-up";
            "Mod+Shift+Down" = action "move-window-to-workspace-down";

            "Mod+Comma" = action "consume-window-into-column";
            "Mod+Period" = action "expel-window-from-column";

            "Mod+R" = action "switch-preset-column-width";
            "Mod+M" = action "maximize-column";
            "Mod+Shift+M" = action "fullscreen-window";

            "Mod+Minus" = action "set-column-width -10%";
            "Mod+Plus" = action "set-column-width +10%";
            "Mod+Shift+Minus" = action "set-window-height -1%";
            "Mod+Shift+Plus" = action "set-window-height +1%";
          };

          input = {
            keyboard.xkb.layout = "us";
            mouse.accel-speed = 1.0;
            touchpad = {
              tap = true;
              dwt = true;
              natural-scroll = true;
              click-method = "clickfinger";
            };
            tablet.map-to-output = "eDP-1";
            touch.map-to-output = "eDP-1";
          };
          outputs."DP-1" = {
            enable = true;
            mode.width = 1920;
            mode.height = 1080;
            position.x = 0;
            position.y = 0;
            mode.refresh = 165.001;
          };
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
        };
      }
    ];
  };
}
