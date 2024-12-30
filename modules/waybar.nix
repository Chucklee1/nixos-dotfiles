_: {
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings = [
      {
        layer = "top";
        position = "bottom";

        modules-left = ["idle_inhibitor"];

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
      @define-color background #303030;
      @define-color text #E0E0E0;

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
