{inputs, ...}: {
  nix.global = [
    inputs.niri.nixosModules.niri
    ({pkgs, ...}: {
      nixpkgs.overlays = [inputs.niri.overlays.niri];

      programs.niri = {
        enable = true;
        package = pkgs.niri-unstable;
      };

      environment.systemPackages = with pkgs; [
        egl-wayland
        qt5.qtwayland
        qt6.qtwayland
        brightnessctl
        wev
        wmenu
        xwayland
        xwayland-run
        wl-color-picker
        wl-clipboard
      ];
      xdg.portal = {
        extraPortals = [pkgs.xdg-desktop-portal-gtk];
        config.common.default = "*";
      };
    })
  ];

  home.global = [
    ({
      lib,
      config,
      pkgs,
      ...
    }:
      with config.lib.stylix.colors.withHashtag; {
        programs.swaylock = {
          enable = true;
          package = pkgs.swaylock-effects;
        };

        programs.niri.settings = with config.lib.niri.actions; let
          mod = "Mod";
          sh = cmd: spawn "sh" "-c" "${cmd}";
          callExe = pkg: lib.getExe pkgs.${pkg};
        in {
          # general
          prefer-no-csd = true;
          hotkey-overlay.skip-at-startup = true;
          screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";
          # env vars
          environment = {
            XDG_CURRENT_DESKTOP = "niri";
            XDG_SESSION_DESKTOP = "niri";
            NIXOS_OZONE_WL = "1";
            MOZ_ENABLE_WAYLAND = "1";
            DISPLAY = ":0";
            _JAVA_AWT_WM_NONREPARENTING = "1";
            SDL_VIDEODRIVER = "x11";
            GDK_BACKEND = "wayland,x11";
            QT_QPA_PLATFORM = "wayland;xcb";
            QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
            QT_AUTO_SCREEN_SCALE_FACTOR = "1";
          };

          spawn-at-startup = [
            {command = ["${callExe "xwayland-satellite"}"];}
            {command = ["${callExe "wlsunset"}" "-T" "5200"];}
            {command = ["${callExe "swaybg"}" "-m" "fill" "-i" "${config.stylix.image}"];}
            {command = ["systemctl" "--user" "reset-failed" "waybar.service"];}
          ];

          switch-events = {
            tablet-mode-on.action = sh "notify-send tablet-mode-on";
            tablet-mode-off.action = sh "notify-send tablet-mode-off";
            lid-open.action = sh ''
              niri msg action power-on-monitors
              swaylock
            '';
            lid-close.action = sh "niri msg action power-off-monitors";
          };

          # keybinds
          binds = {
            # programs
            "${mod}+Return".action = spawn "kitty";
            "${mod}+E".action = spawn "thunar";
            "${mod}+Space".action = sh ''
              wmenu-run -N "${base00}" -n "${base07}" -S "${base0D}" -s "${base00}"
            '';
            "${mod}+Shift+L".action = spawn "swaylock";
            "${mod}+W".action = sh ''systemctl --user restart waybar.service'';
            # clipboard
            "${mod}+Shift+C".action = sh "env DISPLAY=:0 xsel -ob | wl-copy";
            "${mod}+Shift+V".action = sh "wl-paste -n | env DISPLAY=:0 xsel -ib";
            # media keys
            "XF86AudioRaiseVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05+";
            "XF86AudioLowerVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05-";
            "XF86AudioMute".action = sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
            "XF86MonBrightnessUp".action = sh "brightnessctl set 5%+";
            "XF86MonBrightnessDown".action = sh "brightnessctl set 5%-";
            # screenshot
            "Print".action = screenshot;
            "Alt+Print".action = screenshot-window;
            # quits
            "${mod}+Q".action = close-window;
            "Ctrl+Alt+Delete".action = quit;
            "Ctrl+Shift+Alt+Delete".action = quit {skip-confirmation = true;};
            # window focus and move
            # "${modType}+UDLR".action = ${movement}-${node}-UDLR
            "${mod}+Up".action = focus-window-up;
            "${mod}+Down".action = focus-window-down;
            "${mod}+Left".action = focus-column-left;
            "${mod}+Right".action = focus-column-right;
            "${mod}+Shift+Up".action = move-window-up;
            "${mod}+Shift+Down".action = move-window-down;
            "${mod}+Shift+Left".action = move-column-left;
            "${mod}+Shift+Right".action = move-column-right;
            # workspace and monitor move
            "${mod}+Ctrl+up".action = focus-workspace-up;
            "${mod}+Ctrl+down".action = focus-workspace-down;
            "${mod}+Shift+Ctrl+up".action = move-window-to-workspace-up;
            "${mod}+Shift+Ctrl+down".action = move-window-to-workspace-down;
            "${mod}+Alt+left".action = focus-monitor-next;
            "${mod}+Alt+right".action = focus-monitor-previous;
            "${mod}+Shift+Alt+left".action = move-window-to-monitor-next;
            "${mod}+Shift+Alt+right".action = move-window-to-monitor-previous;

            # column width - using = since + needs shift
            "${mod}+Minus".action = set-column-width "-10%";
            "${mod}+Equal".action = set-column-width "+10%";
            "${mod}+Shift+Minus".action = set-column-width "-1%";
            "${mod}+Shift+Equal".action = set-column-width "+1%";
            "${mod}+Ctrl+Minus".action = set-window-height "-10%";
            "${mod}+Ctrl+Equal".action = set-window-height "+10%";
            "${mod}+Shift+Ctrl+Minus".action = set-column-width "-1%";
            "${mod}+Shift+Ctrl+Equal".action = set-column-width "+1%";
            # window presets
            "${mod}+R".action = switch-preset-column-width;
            "${mod}+M".action = expand-column-to-available-width;
            "${mod}+Ctrl+M".action = maximize-column;
            "${mod}+Shift+M".action = fullscreen-window;
            "${mod}+Period".action = consume-or-expel-window-right;
            "${mod}+Comma".action = consume-or-expel-window-left;
            # floating windows
            "${mod}+t".action = toggle-column-tabbed-display;
            "${mod}+f".action = switch-focus-between-floating-and-tiling;
            "${mod}+Shift+f".action = toggle-window-floating;
          };
          # input
          input = {
            keyboard.xkb.layout = "us";
            mouse.accel-speed = 0.0;
            tablet.map-to-output = "eDP-1";
            touch.map-to-output = "eDP-1";
          }; # layout n theming
          layout = {
            gaps = 4;
            border.width = 2;
            always-center-single-column = false;
            tab-indicator = {
              hide-when-single-tab = true;
              position = "top";
            };
          };
          window-rules = let
            r = 4.0;
          in [
            {
              geometry-corner-radius = {
                top-left = r;
                top-right = r;
                bottom-left = r;
                bottom-right = r;
              };
              clip-to-geometry = true;
            }
            {
              matches = [{app-id = "^org.prismlauncher.PrismLauncher$";}];
              open-floating = false;
            }
          ];
        };

        # waybar
        stylix.targets.waybar.enable = false;
        programs.waybar = let
          span = color: str: ''<span color="${color}" >${str}</span>'';
        in {
          enable = true;
          systemd.enable = true;
          settings = [
            {
              position = "top";
              layer = "top";
              height = 20;

              modules-left = [
                "niri/window"
              ];
              modules-center = [
                "clock#date"
                "custom/seperator"
                "clock#time"
              ];
              modules-right = [
                "idle_inhibitor"
                "pulseaudio"
                "network"
                "backlight"
                "battery"
                "tray"
              ];

              "niri/window" = {
                tooltip = false;
              };

              "clock#date" = {
                format = "{:%Y-%M-%d}";
                tooltip = false;
              };
              "custom/seperator" = {
                format = " | ";
                tooltip = false;
              };
              "clock#time" = {
                format = "{:%H:%M:%S}";
                interval = 1;
                tooltip = false;
              };

              idle_inhibitor = {
                format = "{icon}";
                format-icons = {
                  activated = span base0C "";
                  deactivated = "";
                  tooltip = false;
                };
              };
              pulseaudio = {
                format = "{volume}% {icon}";
                format-bluetooth = "{volume}% {icon}";
                format-icons = {"default" = ["" "" ""];};
                format-muted = span base08 "M ";
                format-source = "{volume}% ";
                format-source-muted = span base08 "M ";
                on-click = "pavucontrol";
                tooltip = false;
              };
              network = {
                format-disconnected = "disconnected ⚠";
                format-ethernet = "{ipaddr}/{cidr}";
                format-wifi = "{essid} ({signalStrength}%) ";
                on-click = "nmtui";
                tooltip-format = "{ifname} via {gwaddr}";
              };
              backlight = {
                format = ''{percent}% ${span base0A "{icon}"}'';
                format-icons = ["" "" "" "" "" "" "" "" ""];
              };
              battery = {
                interval = 30;
                states = {
                  warning = 40;
                  critical = 20;
                };
                format-icons = [" " " " " " " " " "];
                format = ''{capacity}% ${span base0B "{icon}"}'';
                format-warning = ''{capacity}% ${span base09 "{icon}"}'';
                format-critical = ''{capacity}% ${span base08 "{icon}"}'';
                format-charging = ''{capacity}% ${span base0B "󱐋{icon}"}'';
                format-charging-warning = ''{capacity}% ${span base09 "󱐋{icon}"}'';
                format-charging-critical = ''{capacity}% ${span base08 "󱐋{icon}"}'';
                format-alt = "{icon} {time}";
                tooltip = false;
              };
            }
          ];
          style =
            # css
            ''
              * {
                font-family: "JetBrainsMono nerd font";
                font-size: 12px;
              }
              #window {
                padding-left: 2px;
              }

              #idle_inhibitor,
              #pulseaudio,
              #network,
              #backlight,
              #battery {
                padding: 0 10px 0 10px;
              }
            '';
        };
      })
  ];

  home.desktop = [
    {
      programs.niri.settings.outputs."DP-1".mode = {
        width = 1920;
        height = 1080;
        refresh = 165.001;
      };
    }
  ];

  home.laptop = [
    {
      programs.niri.settings = {
        input.touchpad = {
          tap = true;
          dwt = true;
          natural-scroll = true;
          click-method = "clickfinger";
        };
        outputs = {
          "eDP-1".position = {
            x = 1920;
            y = 0;
          };
          "DP-5".mode = {
            width = 1920;
            height = 1080;
            refresh = 165.001;
          };
        };
      };
    }
  ];
}
