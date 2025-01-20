{config, ...}: {
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings = [
      {
        position = "top";
        layer = "top";
        height = 20;
        spacing = 4;

        modules-left = [
          "idle_inhibitor"
          "cpu"
          "memory"
          "niri/window"
        ];

        idle_inhibitor = {
          format = "{icon}";
          format-icons = {
            activated = "";
            deactivated = "";
          };
        };

        cpu = {
          format = "{usage}% ";
          tooltip = false;
        };

        memory = {format = "{}% ";};

        modules-center = ["clock"];
        clock = {format = "{:%H:%M:%S}";};

        modules-right = [
          "pulseaudio"
          "network"
          "backlight"
          "power-profiles-daemon"
          "battery"
          "tray"
          "custom/power"
        ];

        pulseaudio = {
          format = "{volume}% {icon} {format_source}";
          format-bluetooth = "{volume}% {icon} {format_source}";
          format-icons = {default = ["" "" ""];};
          format-muted = " {format_source}";
          format-source = "{volume}% ";
          format-source-muted = "";
          on-click = "pavucontrol";
        };

        network = {
          format-alt = "{ifname}: {ipaddr}/{cidr}";
          format-disconnected = "Disconnected ⚠";
          format-ethernet = "{ipaddr}/{cidr} ";
          format-wifi = "{essid} ({signalStrength}%) ";
          tooltip-format = "{ifname} via {gwaddr} ";
        };

        backlight = {
          format = "{percent}% {icon}";
          format-icons = ["" "" "" "" "" "" "" "" ""];
        };

        power-profiles-daemon = {
          format = "{icon}";
          format-icons = {
            balanced = "";
            default = "";
            performance = "";
            power-saver = "";
          };
          tooltip = true;
          tooltip-format = "Power profile: {profile}\nDriver: {driver}";
        };

        battery = {
          format = "{capacity}% {icon}";
          format-alt = "{time} {icon}";
          format-charging = "{capacity}% ";
          format-full = "{capacity}% {icon}";
          format-icons = ["" "" "" "" ""];
          format-plugged = "{capacity}% ";
          states = {
            critical = 15;
            warning = 30;
          };
        };

        tray = {spacing = 10;};

        "custom/power" = {
          format = "⏻ ";
          on-click = "wlogout";
        };
      }
    ];

    style = with config.lib.stylix.colors; let
      black = "#${base00}";
      text = "#${base07}";
      background = "#${base01}";
      foreground = "#${base03}";
    in ''
      * {
        font-family: "JetBrainsMono nerd font";
        font-size: 12px;
      }

      window#waybar {
        background-color: ${background};
        color: ${text};
        transition-property: background-color;
        transition-duration: .5s;
      }

      window#waybar.hidden {
        opacity: 0.2;
      }

      window#waybar.empty {
        background-color: transparent;
      }

      button {
        border: none;
        border-radius: 0;
      }

      button:hover {
        background: inherit;
        box-shadow: inset 0 -3px ${text};
      }

      #workspaces button {
        padding: 0 5px;
        background-color: transparent;
        color: ${text};
      }

      #workspaces button:hover {
        background: ${black};
      }

      #workspaces button.focused {
        background-color: ${foreground};
        box-shadow: inset 0 -3px ${text};
      }

      #workspaces button.urgent {
        background-color: #eb4d4b;
      }

      #clock,
      #battery,
      #cpu,
      #memory,
      #disk,
      #backlight,
      #network,
      #pulseaudio,
      #wireplumber,
      #custom-media,
      #tray,
      #mode,
      #idle_inhibitor,
      #power-profiles-daemon {
          padding: 0 10px;
          color: ${text};
      }

      #window,
      #workspaces {
          margin: 0 4px;
      }

      /* If workspaces is the leftmost module, omit left margin */
      .modules-left > widget:first-child > #workspaces {
          margin-left: 0;
      }

      /* If workspaces is the rightmost module, omit right margin */
      .modules-right > widget:last-child > #workspaces {
          margin-right: 0;
      }

      #battery.critical:not(.charging) {
          background-color: #f53c3c;
          color: ${text};
          animation-name: blink;
          animation-duration: 0.5s;
          animation-timing-function: steps(12);
          animation-iteration-count: infinite;
          animation-direction: alternate;
      }

      #power-profiles-daemon {
          padding-right: 15px;
      }
    '';
  };
}
