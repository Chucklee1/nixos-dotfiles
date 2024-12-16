{
  lib,
  config,
  pkgs,
  inputs,
  defaults,
  ...
}: {
  options.niri.enable = lib.mkEnableOption "enable niri window manager";

  config = lib.mkIf config.niri.enable {
    nixpkgs.overlays = [inputs.niri.overlays.niri];
    programs.niri = {
      enable = true;
      package = pkgs.niri-unstable;
    };

    # nvidia support
    hardware.nvidia.modesetting.enable = lib.mkIf (config.niri.enable && config.nvidia.enable) true;
    environment.variables = lib.mkIf (config.niri.enable && config.nvidia.enable) {
      WLR_NO_HARDWARE_CURSORS = "1";
      GBM_BACKEND = "nvidia_drm";
      __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      LIBVA_DRIVER_NAME = "nvidia";
    };

    home-manager.sharedModules = [
      {
        home.packages = [pkgs.swww];
        programs = {
          fuzzel.enable = true;
          wlogout.enable = true;
        };
        services = {
          gnome-keyring.enable = true;
          dunst.enable = true;
        };

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
            {command = ["sh" "-c" ''swww-daemon && swww img ${defaults.wallpaper}''];}
            {command = ["${lib.getExe pkgs.xwayland-satellite}"];}
            {command = ["${pkgs.lxqt.lxqt-policykit}"];}
            {command = ["${lib.getExe pkgs.networkmanagerapplet}"];}
            {command = ["${lib.getExe pkgs.wlsunset}" "sh" "-c" ''-T 5500''];}
          ];

          binds = let
            spawn = command: {action.spawn = ["sh" "-c" ''${command}''];};
            action = command: {action.spawn = ["sh" "-c" ''niri msg action ${command}''];};
          in {
            # programs
            "Mod+Return" = spawn "${defaults.terminal}";
            "Mod+E" = spawn "${defaults.file-manager}";
            "Mod+Space" = spawn "fuzzel";
            "Super+Shift+L" = spawn "swaylock";
            "Super+Shift+P" = spawn "wlogout";

            # media keys
            "XF86AudioRaiseVolume" = spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+";
            "XF86AudioLowerVolume" = spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-";
            "XF86AudioMute" = spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
            "XF86AudioMicMute" = spawn "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle";
            "XF86MonBrightnessUp" = spawn "${lib.getExe pkgs.brightnessctl} --device=amdgpu_bl1 s 5%+";
            "XF86MonBrightnessDown" = spawn "${lib.getExe pkgs.brightnessctl} --device=amdgpu_bl1 s 5%-";

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

            "Mod+R" = action "switch-preset-column-width";
            "Mod+M" = action "maximize-column";
            "Mod+Shift+M" = action "fullscreen-window";
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
          outputs."HKC OVERSEAS LIMITED 24E4 0000000000001" = {
            enable = true;
            mode.width = 1920;
            mode.height = 1080;
            position.x = 1920;
            position.y = 0;
            mode.refresh = 165.001;
          };
          outputs."eDP-1" = {
            enable = true;
            mode.width = 1920;
            mode.height = 1080;
            position.x = 0;
            position.y = 0;
            mode.refresh = 60.008;
          };
          layout = {
            gaps = 0;
            border.width = 2;
            always-center-single-column = false;
          };
        };

        programs.waybar = {
          enable = true;
          systemd.enable = true;
          settings = [
            {
              layer = "top";
              position = "bottom";

              modules-left = ["niri/workspaces" "idle_inhibitor"];

              "niri/workspaces" = {
                format = "{icon}";
                "format-icons" = {
                  focused = "󰻀";
                  default = "";
                };
              };

              "idle_inhibitor" = {
                format = "{icon}";
                "format-icons" = {
                  activated = "";
                  deactivated = "";
                };
              };

              modules-center = ["clock#1" "custom/divider" "clock#2"];

              "clock#1" = {
                format = "{:%H:%M:%S}";
                tooltip = false;
                interval = 1;
              };

              "custom/divider" = {
                format = "|";
                tooltip = false;
              };

              "clock#2" = {
                format = "{:%m.%d.%y}";
                tooltip = "true";
                interval = 60;
              };

              modules-right = [
                "pulseaudio"
                "memory"
                "cpu"
                "disk"
                "backlight"
                "battery"
                "tray"
                "custom/power"
              ];

              "pulseaudio" = {
                format = "{volume:2}% {icon}";
                "format-bluetooth" = "{volume}% {icon}";
                "format-muted" = "{volume}% 󰝟";
                "format-icons" = {
                  headphones = "";
                  default = ["" ""];
                };
                "scroll-step" = 5;
                "on-click" = "pamixer -t";
                "on-click-right" = "pavucontrol";
              };

              "memory" = {
                interval = 5;
                format = "{}% ";
              };

              "cpu" = {
                interval = 5;
                format = "{usage:2}% ";
              };

              "disk" = {
                interval = 5;
                format = "{percentage_used:2}% ";
                path = "/";
              };

              "backlight" = {
                device = "amdgpu_bl1e";
                format = "{percent}% {icon}";
                "format-icons" = ["" "" "" "" "" "" "" "" ""];
              };

              "battery" = {
                states = {
                  good = 95;
                  warning = 30;
                  critical = 15;
                };
                format = "{capacity}% {icon}";
                "format-icons" = ["" "" "" "" ""];
              };

              "tray" = {};

              "custom/power" = {
                format = "⏻";
                "on-click" = "wlogout";
              };
            }
          ];
          style = ''
            @define-color background ${defaults.colors.base02};
            @define-color text ${defaults.colors.base06};

            * {
              font-family: "Nerd Fonts Symbols Only", "Ariel", sans-serif;
              font-size: 11px;
            }

            window#waybar {
                background: @background;
                color: @text;
            }

            #workspaces { background: @background; }
            #workspaces button.focused { color: @text; }

            #workspaces button {
                padding: 0 2px;
                color: @text;
            }


            #workspaces button:hover {
                box-shadow: inherit;
                text-shadow: inherit;
            }
            #workspaces button:hover {
                background: @background;
                padding: 0 3px;
            }

            #custom-divider {
              background: @background;
              color: @text;
              padding: 0 0;
            }

            #idle_inhibitor,
            #clock,
            #pulseaudio,
            #memory,
            #cpu,
            #disk,
            #battery,
            #tray,
            #backlight,
            #custom-power {
                color: @text;
                padding: 0 10px;
            }
          '';
        };
      }
    ];
  };
}
