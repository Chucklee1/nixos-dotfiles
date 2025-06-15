{
  inputs,
  system,
  ...
}: {
  home.global = [
    ({
      lib,
      config,
      pkgs,
      ...
    }: {
      # waybar
      nixpkgs.overlays = [(_: _: {waybar_git = inputs.waybar.packages.${system}.waybar;})];
      stylix.targets.waybar.enable = false;
      programs.waybar = with config.lib.stylix.colors.withHashtag; let
        span = color: str: ''<span color="${color}" >${str}</span>'';
      in {
        enable = true;
        package = pkgs.waybar_git;
        systemd.enable = true;
        settings = [
          {
            position = "top";
            layer = "top";
            height = 20;

            modules-left = [
              "mpd"
            ];
            modules-center = [
              "clock"
            ];
            modules-right = [
              "idle_inhibitor"
              "pulseaudio"
              "custom/tailscale"
              "network"
              "backlight"
              "battery"
              "tray"
            ];

            mpd = {
              "format" = "{stateIcon} {consumeIcon}{randomIcon}{repeatIcon}{singleIcon}{artist} - {album} - {title} ({elapsedTime:%M:%S}/{totalTime:%M:%S}) ";
              "format-disconnected" = "Disconnected ";
              "format-stopped" = "{consumeIcon}{randomIcon}{repeatIcon}{singleIcon}Stopped ";
              "interval" = 10;
              "consume-icons" = {
                "on" = " "; # Icon shows only when "consume" is on
              };
              "random-icons" = {
                "off" = "<span color=\"#f53c3c\"></span> "; # Icon grayed out when "random" is off
                "on" = " ";
              };
              "repeat-icons" = {
                "on" = " ";
              };
              "single-icons" = {
                "on" = "1 ";
              };
              "state-icons" = {
                "paused" = "";
                "playing" = "";
              };
              "tooltip-format" = "MPD (connected)";
              "tooltip-format-disconnected" = "MPD (disconnected)";
            };
            "niri/window" = {
              tooltip = false;
              format = "{app_id}";
            };

            "clock" = {
              format = "{:%F | %X}";
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
              tooltip = false;
            };
            network = {
              format-disconnected = "nah ⚠";
              format-ethernet = "{ipaddr}/{cidr}";
              format-wifi = "{essid} ({signalStrength}%) ";
              tooltip = false;
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
          lib.mkAfter
          # css
          ''
            * {
              font-family: "JetBrainsMono nerd font";
              font-size: 12px;
            }

            #window {
              padding-left: 2;
            }

            #idle_inhibitor,
            #pulseaudio,
            #network,
            #backlight,
            #battery {
              padding: 0 10 0 10;
            }
          '';
      };
    })
  ];
}
