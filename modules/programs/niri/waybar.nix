{
  lib,
  config,
  ...
}: {
  config = lib.mkIf config.niri.enable {
    home-manager.sharedModules = [
      {
        programs.waybar = {
          enable = true;
          systemd.enable = true;
        };
        home.file.".config/waybar/config.jsonc".text = ''
          {
              "layer": "top",
              "position": "top",

              "modules-left": [
                  "niri/workspaces",
                  "idle_inhibitor",
                  "custom/down-right-arrow"
              ],
              "modules-center": [
                  "custom/up-left-arrow",
                  "clock#1",
                  "clock#2",
                  "clock#3",
                  "custom/up-right-arrow"
              ],
              "modules-right": [
                  "custom/down-left-arrow",
                  "pulseaudio",
                  "memory",
                  "cpu",
                  "disk",
                  "backlight",
                  "battery",
                  "tray",
                  "custom/power"
              ],

              "custom/up-left-arrow": {
                  "format": "",
                  "tooltip": false
              },
              "custom/up-right-arrow": {
                  "format": "",
                  "tooltip": false
              },
              "custom/down-left-arrow": {
                  "format": "",
                  "tooltip": false
              },
              "custom/down-right-arrow": {
                  "format": "",
                  "tooltip": false
              },

              "niri/workspaces": {
                  "format": "{icon}",
                  "format-icons": { "focused": "󰻀", "default": "" }
              },
              "idle_inhibitor": {
                  "format": "{icon}",
                  "format-icons": {
                  "activated": "",
                  "deactivated": ""
              }
              },

              "clock#1": {
                  "format": "{:%a}",
                  "tooltip": false
              },
              "clock#2": {
                  "format": "{:%H:%M}",
                  "tooltip": false
              },
              "clock#3": {
                  "format": "{:%m-%d}",
                  "tooltip": false
              },

              "pulseaudio": {
                  "format": "{volume:2}% {icon} ",
                  "format-bluetooth": "{volume}% {icon}",
                  "format-muted": "M 󰝟",
                  "format-icons": {
                      "headphones": "",
                      "default": [ "", ""]
                  },
                  "scroll-step": 5,
                  "on-click": "pamixer -t",
                  "on-click-right": "pavucontrol"
              },
              "memory": {
                  "interval": 5,
                  "format": "{}% "
              },
              "cpu": {
                  "interval": 5,
                  "format": "{usage:2}% "
              },
              "disk": {
                  "interval": 5,
                  "format": "{percentage_used:2}% ",
                  "path": "/"
              },
              "backlight": {
                  "device": "amdgpu_bl1e",
                  "format": "{percent}% {icon}",
                  "format-icons": ["", "", "", "", "", "", "", "", ""]
              },
              "battery": {
                  "states": {
                      "good": 95,
                      "warning": 30,
                      "critical": 15
                  },
                  "format": "{capacity}% {icon}",
                      "format-icons": [
                          "",
                          "",
                          "",
                          "",
                          ""
                      ]
              },
              "tray": {},
              "custom/power": {
                  "format": "⏻",
                  "on-click": "wlogout"
              }
          }
        '';

        home.file.".config/waybar/style.css".text = ''
          @define-color background #292b2e;
          @define-color foreground #1a1a1a;
          @define-color text #fdf6e3;
          @define-color text-blue #268bd2;

          * {
              font-family: "Nerd Fonts Symbols Only", "Ariel", sans-serif;
              font-size: 12px;
          }

          window#waybar {
              background: @background;
              color: @text;
          }

          #custom-up-left-arrow,
          #custom-up-right-arrow,
          #custom-down-left-arrow,
          #custom-down-right-arrow {
              color: @foreground;
              font-size: 27px;
          }

          #workspaces,
          #idle_inhibitor,
          #clock,
          #pulseaudio,
          #memory,
          #cpu,
          #disk,
          #backlight,
          #battery,
          #tray,
          #custom-power {
              background: @foreground;
          }

          #workspaces button {
              padding: 0 2px;
              color: @text;
          }
          #workspaces button.focused {
              color: @text-blue;
          }
          #workspaces button:hover {
              box-shadow: inherit;
              text-shadow: inherit;
          }
          #workspaces button:hover {
              background: @foreground;
              border: @foreground;
              padding: 0 3px;
          }

          #custom-power,
          #pulseaudio {
              color: @text-blue;
          }
          #memory {
              color: #2aa198;
          }
          #cpu {
              color: #6c71c4;
          }
          #disk {
              color: #b58900;
          }
          #battery {
              color: #859900;
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
              padding: 0 10px;
          }
        '';
      }
    ];
  };
}
