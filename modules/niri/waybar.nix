{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    style = ''
      ${builtins.readFile "${pkgs.waybar}/etc/xdg/waybar/style.css"}
      @define-color base00 #171D23;
      @define-color base01 #1D252C;
      @define-color base02 #28323A;
      @define-color base03 #526270;
      @define-color base04 #B7C5D3;
      @define-color base05 #D8E2EC;
      @define-color base06 #F6F6F8;
      @define-color base07 #FBFBFD;
      @define-color base08 #D95468;
      @define-color base09 #FF9E64;
      @define-color base0A #EBBF83;
      @define-color base0B #8BD49C;
      @define-color base0C #70E1E8;
      @define-color base0D #539AFC;
      @define-color base0E #B62D65;
      @define-color base0F #DD9D82;

      * {
        font-size: 14px;
        font-family: "Noto Nerd Font";
      }

      window#waybar {
        background: @base02;
        color: @base05;
      }

      #custom-right-arrow-dark,
      #custom-left-arrow-dark {
        color: @base00;
      }
      #custom-right-arrow-light,
      #custom-left-arrow-light {
        color: @base02;
        background: @base00;
      }

      #workspaces,
      #clock.1,
      #clock.2,
      #clock.3,
      #pulseaudio,
      #memory,
      #cpu,
      #battery,
      #disk,
      #tray {
        background: @base00;
      }

      #workspaces button {
        padding: 0 2px;
        color: @base05;
      }
      #workspaces button.focused {
        color: @base0D;
      }
      #workspaces button:hover {
        box-shadow: inherit;
        text-shadow: inherit;
      }
      #workspaces button:hover {
        background: @base00;
        border: @base00;
        padding: 0 3px;
      }

      #pulseaudio {
        color: @base0D;
      }
      #memory {
        color: @base0C;
      }
      #cpu {
        color: @base0E;
      }
      #battery {
        color: @base0B;
      }
      #disk {
        color: @base0A;
      }

      #clock,
      #pulseaudio,
      #memory,
      #cpu,
      #battery,
      #disk {
        padding: 0 10px;
      }
    '';
    settings = [
      {
        height = 30;
        layer = "top";
        position = "top";

        modules-left = [
          "niri/workspaces"
          "custom/right-arrow-dark"
        ];
        modules-center = [
          "custom/left-arrow-dark"
          "clock#1"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "clock#2"
          "custom/right-arrow-dark"
          "custom/right-arrow-light"
          "clock#3"
          "custom/right-arrow-dark"
        ];
        modules-right = [
          "custom/left-arrow-dark"
          "pulseaudio"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "memory"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "cpu"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "battery"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "disk"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "tray"
        ];

        custom = {
          "left-arrow-dark" = {
            format = "";
            tooltip = false;
          };
          "left-arrow-light" = {
            format = "";
            tooltip = false;
          };
          "right-arrow-dark" = {
            format = "";
            tooltip = false;
          };
          "right-arrow-light" = {
            format = "";
            tooltip = false;
          };
        };

        "niri/workspaces" = {
          format = "{icon}";
          format-icons = {
            active = "󰻀";
            default = "";
          };
        };

        "clock#1" = {
          format = "{:%a}";
          tooltip = false;
        };
        "clock#2" = {
          format = "{:%H:%M}";
          tooltip = false;
        };
        "clock#3" = {
          format = "{:%m-%d}";
          tooltip = false;
        };

        pulseaudio = {
          format = "{icon} {volume:2}%";
          format-bluetooth = "{icon}  {volume}%";
          format-muted = "MUTE";
          format-icons = {
            headphones = "";
            default = ["" ""];
          };
          scroll-step = 5;
          on-click = "pamixer -t";
          on-click-right = "pavucontrol";
        };

        memory = {
          interval = 5;
          format = "Mem {}%";
        };

        cpu = {
          interval = 5;
          format = "CPU {usage:2}%";
        };

        battery = {
          states = {
            good = 95;
            warning = 30;
            critical = 15;
          };
          format = "{icon} {capacity}%";
          format-icons = ["" "" "" "" ""];
        };

        disk = {
          interval = 5;
          format = "Disk {percentage_used:2}%";
          path = "/";
        };

        tray = {
          "icon-size" = 20;
        };
      }
    ];
  };
}
