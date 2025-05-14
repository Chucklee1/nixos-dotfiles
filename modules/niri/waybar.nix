{
  inputs,
  system,
  self,
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
              "niri/window"
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

            "niri/window" = {
              tooltip = false;
              format = "{app_id}";
            };

            "clock" = {
              format = "{:%Y-%M-%d | %H:%M:%S}";
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
            "custom/tailscale" = {
              exec = "exec ${self}/assets/waybar-tailscale.sh --status";
              on-click = "exec ${self}/assets/waybar-tailscale.sh --toggle";
              exec-on-event = true;
              format = "VPN: {icon}";
              format-icons = {
                connected = "on";
                stopped = "off";
              };
              tooltip = true;
              return-type = "json";
              interval = 3;
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
