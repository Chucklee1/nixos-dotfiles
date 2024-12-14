{
  config,
  lib,
  ...
}: let
  # classic dark
  base00 = "#151515";
  base01 = "#202020";
  base02 = "#303030";
  base03 = "#505050";
  base04 = "#B0B0B0";
  base05 = "#D0D0D0";
  base06 = "#E0E0E0";
  base07 = "#F5F5F5";
  base08 = "#AC4142";
  base09 = "#D28445";
  base0A = "#F4BF75";
  base0B = "#90A959";
  base0C = "#75B5AA";
  base0D = "#6A9FB5";
  base0E = "#AA759F";
  base0F = "#8F5536";
in {
  options = {
    theme-minimal.enable = lib.mkEnableOption "enable minimal theme";
  };

  config = {
    stylix = lib.mkIf config.theme-minimal.enable {
      opacity.terminal = 1.0;
      base16Scheme = {
        base00 = base00;
        base01 = base01;
        base02 = base02;
        base03 = base03;
        base04 = base04;
        base05 = base05;
        base06 = base06;
        base07 = base07;
        base08 = base08;
        base09 = base09;
        base0A = base0A;
        base0B = base0B;
        base0C = base0C;
        base0D = base0D;
        base0E = base0E;
        base0F = base0F;
      };
    };

    home-manager.sharedModules = [
      {
        programs.niri.settings.layout = lib.mkIf (config.theme-minimal.enable && config.niri.enable) {
          gaps = 0;
          border.width = 2;
          always-center-single-column = false;
        };

        programs.waybar = lib.mkIf config.theme-minimal.enable {
          enable = true;
          systemd.enable = true;
          settings = [
            {
              layer = "top";
              position = "bottom";

              modules-left = [
                "niri/workspaces"
                "idle_inhibitor"
              ];

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

              modules-center = [
                "clock#1"
                "custom/divider"
                "clock#2"
              ];

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
                "format-icons" = [
                  ""
                  ""
                  ""
                  ""
                  ""
                  ""
                  ""
                  ""
                  ""
                ];
              };

              "battery" = {
                states = {
                  good = 95;
                  warning = 30;
                  critical = 15;
                };
                format = "{capacity}% {icon}";
                "format-icons" = [
                  ""
                  ""
                  ""
                  ""
                  ""
                ];
              };

              "tray" = {};

              "custom/power" = {
                format = "⏻";
                "on-click" = "wlogout";
              };
            }
          ];
          style = ''
            @define-color background ${base02};
            @define-color text ${base06};

            * {
                font-family: "Nerd Fonts Symbols Only", "Ariel", sans-serif;
                font-size: 11px;
            }

            window#waybar {
                background: @background;
                color: @text;
            }

            #workspaces {
                background: @background;
            }

            #workspaces button {
                padding: 0 2px;
                color: @text;
            }
            #workspaces button.focused {
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
