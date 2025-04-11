{
  home.global = [
    ({
      lib,
      config,
      ...
    }: {
      config = (lib.mkIf config.host.machine != "darwin") {
        stylix.targets.waybar.enable = false;
        programs.waybar = with config.lib.stylix.colors.withHashtag; let
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
                on-click = "kitty -e nmtui";
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
      };
    })
  ];
}
