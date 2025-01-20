{
  waybar = {
    enable = true;
    systemd.enable = true;
    settings = {
      position = "top";
      layer = "top";
      height = 30;
      spacing = 4;

      modules-left = [
        "niri/workspaces"
        "niri/window"
        "custom/media"
      ];
      "custom/media" = {
        escape = true;
        exec = "$HOME/.config/waybar/mediaplayer.py --player spotify 2> /dev/null";
        format = "{icon} {text}";
        format-icons = {
          default = "🎜";
          spotify = "";
        };
        max-length = 40;
        return-type = "json";
      };

      modules-center = [
        "clock"
      ];
      clock = {format = "{:%H:%M:%S}";};

      modules-right = [
        "idle_inhibitor"
        "pulseaudio"
        "network"
        "power-profiles-daemon"
        "cpu"
        "memory"
        "backlight"
        "battery"
        "tray"
        "custom/power"
      ];

      idle_inhibitor = {
        format = "{icon}";
        format-icons = {
          activated = "";
          deactivated = "";
        };
      };

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

      cpu = {
        format = "{usage}% ";
        tooltip = false;
      };

      memory = {format = "{}% ";};

      backlight = {
        format = "{percent}% {icon}";
        format-icons = ["" "" "" "" "" "" "" "" ""];
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
    };

    style = ''
      * {
        font-family: "JetBrainsMono nerd font";
        font-size: 13px;
      }

      window#waybar {
        background-color: rgba(43, 48, 59, 0.5);
        color: #ffffff;
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
        box-shadow: inset 0 -3px #ffffff;
      }

      #workspaces button {
        padding: 0 5px;
        background-color: transparent;
        color: #ffffff;
      }

      #workspaces button:hover {
        background: rgba(0, 0, 0, 0.2);
      }

      #workspaces button.focused {
        background-color: #64727D;
        box-shadow: inset 0 -3px #ffffff;
      }

      #workspaces button.urgent {
        background-color: #eb4d4b;
      }

      #mode {
        background-color: #64727D;
        box-shadow: inset 0 -3px #ffffff;
      }

      #clock,
      #battery,
      #cpu,
      #memory,
      #disk,
      #temperature,
      #backlight,
      #network,
      #pulseaudio,
      #wireplumber,
      #custom-media,
      #tray,
      #mode,
      #idle_inhibitor,
      #scratchpad,
      #power-profiles-daemon,
      #mpd {
          padding: 0 10px;
          color: #ffffff;
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
          color: #ffffff;
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
