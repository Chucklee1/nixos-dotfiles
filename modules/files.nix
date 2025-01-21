{config, lib, ...}: let
  colorWithHash = lib.mapAttrs (name: color: "#${color}") config.lib.stylix.colors;
in
  with colorWithHash; {
    waybarConfig = builtins.fromJSON (builtins.unsafeDiscardStringContext ''
      [
      {
        "position": "top",
        "layer": "top",
        "height": 20,
        "spacing": 4,

        "modules-left": [
          "idle_inhibitor",
          "cpu",
          "memory",
          "niri/window"
        ],

        "idle_inhibitor": {
          "format": "{icon}",
          "format-icons": {
            "activated": "",
            "deactivated": ""
          }
        },
        "cpu": {
          "format": "{usage}% "
        },
        "memory": {
          "format": "{}% "
        },

        "modules-center": [
          "clock"
        ],

        "clock": {
          "format": "{:%H:%M:%S}"
        },

        "modules-right": [
          "pulseaudio",
          "network",
          "backlight",
          "battery",
          "tray",
          "custom/power"
        ],

        "pulseaudio": {
          "format": "{volume}% {icon} {format_source}",
          "format-bluetooth": "{volume}% {icon} {format_source}",
          "format-icons": {
            "default": [
              "",
              "",
              ""
            ]
          },
          "format-muted": " {format_source}",
          "format-source": "{volume}% ",
          "format-source-muted": "",
          "on-click": "pavucontrol"
        },
        "network": {
          "format-alt": "{ifname}: {ipaddr}/{cidr}",
          "format-disconnected": "Disconnected ⚠",
          "format-ethernet": "{ipaddr}/{cidr} ",
          "format-wifi": "{essid} ({signalStrength}%) ",
          "tooltip-format": "{ifname} via {gwaddr} "
        },
        "backlight": {
          "format": "{percent}% {icon}",
          "format-icons": [
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            ""
          ]
        },
        "battery": {
          "format": "{capacity}% {icon}",
          "format-alt": "{time} {icon}",
          "format-charging": "{capacity}% ",
          "format-full": "{capacity}% {icon}",
          "format-icons": [
            "",
            "",
            "",
            "",
            ""
          ],
          "format-plugged": "{capacity}% ",
          "states": {
            "critical": 15,
            "warning": 30
          }
        },
        "tray": {
          "spacing": 10
        },
        "custom/power": {
          "format": "⏻ ",
          "on-click": "wlogout"
        }
      }
      ]
    '');

    waybarStyle = ''
      * {
        font-family: "JetBrainsMono nerd font";
        font-size: 12px;
      }

      window#waybar {
        color: ${base07};
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
        box-shadow: inset 0 -3px ${base07};
      }

      #workspaces button {
        padding: 0 5px;
        background-color: transparent;
        color: ${base07};
      }

      #workspaces button:hover {
        background: ${base00};
      }

      #workspaces button.focused {
        background-color: ${base03};
        box-shadow: inset 0 -3px ${base07};
      }

      #workspaces button.urgent {
        background-color: ${base08};
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
          color: ${base07};
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
          background-color: ${base08};
          color: ${base07};
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

    custom-pure = builtins.fromJSON (builtins.unsafeDiscardStringContext ''
      {
        "$schema": "https://raw.githubusercontent.com/JanDeDobbeleer/oh-my-posh/main/themes/schema.json",
        "blocks": [
          {
            "alignment": "left",
            "segments": [
              {
                "foreground": "${base08}",
                "style": "plain",
                "template": "{{ .UserName }} ",
                "type": "session"
              },
              {
                "foreground": "${base0D}",
                "properties": {
                  "style": "full"
                },
                "style": "plain",
                "template": "{{ .Path }} ",
                "type": "path"
              }
            ],
            "type": "prompt"
          },
          {
            "alignment": "left",
            "segments": [
              {
                "foreground": "${base04}",
                "properties": {
                  "branch_ahead_icon": "<${base0C}>\u21e1 </>",
                  "branch_behind_icon": "<${base0C}>\u21e3 </>",
                  "branch_icon": "",
                  "fetch_stash_count": true,
                  "fetch_status": true,
                  "fetch_upstream_icon": true,
                  "github_icon": ""
                },
                "style": "plain",
                "template": "{{ .UpstreamIcon }}{{ .HEAD }}{{if .BranchStatus }} {{ .BranchStatus }}{{ end }}{{ if .Working.Changed }}<${base09}>*</>{{ .Working.String }}{{ end }}{{ if and (.Working.Changed) (.Staging.Changed) }} |{{ end }}{{ if .Staging.Changed }} \uf046 {{ .Staging.String }}{{ end }}{{ if gt .StashCount 0 }} \ueb4b {{ .StashCount }}{{ end }} ",
                "type": "git"
              }
            ],
            "type": "prompt"
          },
          {
            "alignment": "left",
            "segments": [
              {
                "foreground": "${base0B}",
                "properties": {
                  "style": "austin"
                },
                "style": "plain",
                "template": " {{ .FormattedMs }} ",
                "type": "executiontime"
              }
            ],
            "type": "prompt"
          },
          {
            "alignment": "left",
            "newline": true,
            "segments": [
              {
                "foreground": "${base0E}",
                "foreground_templates": [
                  "{{ if gt .Code 0 }}${base08}{{ end }}"
                ],
                "properties": {
                  "always_enabled": true
                },
                "style": "plain",
                "template": "\u276f ",
                "type": "status"
              }
            ],
            "type": "prompt"
          }
        ],
        "console_title_template": "{{if .Root}}(Admin){{end}} {{.PWD}}",
        "transient_prompt": {
          "foreground": "${base0E}",
          "foreground_templates": [
            "{{ if gt .Code 0 }}${base08}{{ end }}"
          ],
          "template": "\u276f "
        },
        "version": 3
      }
    '');
  }
