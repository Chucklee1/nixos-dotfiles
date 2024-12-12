{
  config,
  lib,
  ...
}: {
  options = {
    theme-minimal.enable = lib.mkEnableOption "enable minimal theme";
  };

  config = lib.mkIf config.theme-minimal.enable {
    stylix = {
      opacity.terminal = 1.0;
      base16Scheme = {
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
      };
    };
    home-manager.sharedModules = [
      {
        home.file.".config/waybar/config.jsonc".text = ''
          {
            "layer": "top",
            "position": "bottom",

            "modules-left": ["niri/workspaces", "idle_inhibitor"],
            "modules-center": ["clock"],
            "modules-right": [
              "pulseaudio",
              "memory",
              "cpu",
              "disk",
              "backlight",
              "battery",
              "tray",
              "custom/power"
            ],

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

            "clock": {
              "format": "{:%H:%M:%S}",
              "tooltip": false
            },

            "pulseaudio": {
              "format": "{volume:2}% {icon} ",
              "format-bluetooth": "{volume}% {icon}",
              "format-icons": {
                "headphones": "",
                "default": [ "󰝟", "", ""]
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
                "full": 99,
                "good": 98,
                "normal": 98,
                "warning": 20,
                "critical": 20
              },
              "format": "{capacity}% {icon}",
              "format-good": "{icon} {capacity}%",
              "format-full": "   {capacity}%",
              "format-icons": ["", "", "", "", ""],
              "interval": 30
            },
            "tray": {},
            "custom/power": {
              "format": "⏻",
              "on-click": "wlogout"
            }
          }
        '';
        home.file.".config/waybar/style.css".text = let
          background = "#292b2e";
          text = "#fdf6e3";
        in ''
          * {
            font-family: "Nerd Fonts Symbols Only", "Ariel", sans-serif;
            font-size: 10px;
          }

          window#waybar {
            background: ${background};
            color: ${text};
          }

          #workspaces
          #workspaces button {
            color: ${text};
            padding: 0 2px;
          }
          #workspaces button:hover {
            box-shadow: inherit;
            text-shadow: inherit;
          }
          #workspaces button:hover {
            padding: 0 3px;
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
            color: ${text};
            padding: 0 10px;
          }
        '';
      }
    ];
  };
}
