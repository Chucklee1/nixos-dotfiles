{inputs, ...}: let
  nixWaybar = [({system, ...}: {nixpkgs.overlays = [(_: _: {waybar_git = inputs.waybar.packages.${system}.waybar;})];})];
  homeWaybar = [
    ({
      lib,
      config,
      pkgs,
      system,
      ...
    }: {
      # waybar
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
              "idle_inhibitor"
              "niri/window"
            ];
            modules-center = [
              "clock"
            ];
            modules-right = [
              "mpd"
              "pulseaudio"
              "network"
              "backlight"
              "battery"
              "tray"
            ];

            mpd = {
              format = "{stateIcon} {artist}/{title} {elapsedTime:%M:%S}/{totalTime:%M:%S} ";
              format-disconnected = "󰝛";
              format-stopped = "";
              interval = 10;
              state-icons = {
                paused = "";
                playing = "";
              };
              tooltip-format = "MPD (connected)";
              tooltip-format-disconnected = "MPD (disconnected)";
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
in {
  nix.desktop = nixWaybar;
  nix.laptop = nixWaybar;
  home.desktop = homeWaybar;
  home.laptop = homeWaybar;
}
