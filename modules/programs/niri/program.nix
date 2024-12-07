{
  lib,
  config,
  pkgs,
  inputs,
  wallpaper,
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
        programs.niri.settings = {
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
            "sh"
            "-c"
            ''
              lxqt-policykit
              dunst
              xwayland-satellite
              nm-applet
              waybar
              wlsunset -t 5000 -T 6500
              swww-daemon
              swww img ${wallpaper}
            ''
          ];

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
            spawn = args: {"${args [0]}".action.spawn = ["sh" "-c" ''${args [1]}''];};
            action = args: {"${args [0]}".action.spawn = ["sh" "-c" ''niri msg action ${args [1]}''];};
          in {
            spawn = [
              # programs
              ["Mod+Return" "kitty --working-directory ~/nixos-dotfiles"]
              ["Mod+Space" "fuzzel"]
              ["Super+Alt+L" "swaylock"]
              ["Super+Alt+P" "wlogout"]

              # media keys
              ["XF86AudioRaiseVolume" "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+"]
              ["XF86AudioLowerVolume" "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-"]
              ["XF86AudioMute" "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"]
              ["XF86AudioMicMute" "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"]
              ["XF86MonBrightnessUp" "brightnessctl --device=amdgpu_bl1 s 5%+"]
              ["XF86MonBrightNessDown" "brightnessctl --device=amdgpu_bl1 s 5%-"]
            ];

            action = [
              # screenshot
              ["Print" "screenshot"]
              ["Ctrl+Print" "screenshot-screen"]
              ["Alt+Print" "screenshot-window"]

              # window actions - closure
              ["Mod+Q" "close-window"]
              ["Ctrl+Alt+Delete" "quit"]
              # window actions - focus
              ["Mod+Left" "focus-column-left"]
              ["Mod+Right" "focus-column-right"]
              ["Mod+Up" "focus-workspace-up"]
              ["Mod+Down" "focus-workspace-down"]
              # window actions - movement
              ["Mod+Shift+Left" "move-column-left"]
              ["Mod+Shift+Right" "move-column-right"]
              ["Mod+Shift+Up" "move-window-to-workspace-up"]
              ["]Mod+Shift+Down" "move-window-to-workspace-down"]
              # window actions - colume merging
              ["Mod+Comma" "consume-window-into-column"]
              ["Mod+Period" "expel-window-from-column"]
              # window actions - window presets
              ["Mod+R" "switch-preset-column-width"]
              ["Mod+M" "maximize-column"]
              ["Mod+Shift+M" "fullscreen-window"]
              # window actions - precise column widths
              ["Mod+Minus" "set-column-width -10%"]
              ["Mod+Plus" "set-column-width +10%"]
              ["Mod+Shift+Minus" "set-window-width -1%"]
              ["Mod+Shift+Plus" "set-window-width +1%"]
            ];
          };
        };
      }
    ];
  };
}
