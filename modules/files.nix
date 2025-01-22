/*
colorscheme reference:
"Classic Dark", "Jason Heeris (http://heeris.id.au)"
base00: #151515
base01: #202020
base02: #303030
base03: #505050
base04: #B0B0B0
base05: #D0D0D0
base06: #E0E0E0
base07: #F5F5F5
base08: #AC4142
base09: #D28445
base0A: #F4BF75
base0B: #90A959
base0C: #75B5AA
base0D: #6A9FB5
base0E: #AA759F
base0F: #8F5536
*/
{
  config,
  lib,
  ...
}: let
  colorWithHash = lib.mapAttrs (name: color: "#${color}") config.lib.stylix.colors;
in
  with colorWithHash; {
    waybarConfig = builtins.fromJSON (builtins.unsafeDiscardStringContext ''
      [
        {
          "position": "top",
          "layer": "top",
          "height": 20,

          "modules-left": ["niri/window"],

          "cpu": { "format": "{usage}% " },
          "memory": { "format": "{}% " },

          "modules-center": ["clock"],
          "clock": { "format": "{:%H:%M:%S}" },

          "modules-right": [
            "idle_inhibitor",
            "pulseaudio",
            "network",
            "backlight",
            "battery",
            "tray",
            "custom/power"
          ],

          "idle_inhibitor": {
            "format": "{icon}",
            "format-icons": {
              "activated": "",
              "deactivated": ""
            }
          },
          "pulseaudio": {
            "format": "{volume}% {icon}",
            "format-bluetooth": "{volume}% {icon}",
            "format-icons": { "default": ["", "", ""] },
            "format-muted": "<span color='${base08}' >M </span>",
            "format-source": "{volume}% ",
            "format-source-muted": "",
            "on-click": "pavucontrol"
          },
          "network": {
            "format-alt": "{ifname}: {ipaddr}/{cidr}",
            "format-disconnected": "<span color='${base0A}' > disconnected ⚠</span>",
            "format-ethernet": "{ipaddr}/{cidr} 󰊗",
            "format-wifi": "{essid} ({signalStrength}%) ",
            "tooltip-format": "{ifname} via {gwaddr} 󰊗"
          },
          "backlight": {
            "format": "{percent}%<span color='${base0A}' > {icon}</span>",
            "format-icons": ["", "", "", "", "", "", "", "", ""]
          },
          "battery": {
            "interval": 1,
            "states": {
              "good": 99,
              "warning": 30,
              "critical": 20
            },
            "format-icons": [" ", " ", " ", " ", " "],
            "format": "{capacity}%<span color='${base0B}' > {icon}</span>",
            "format-critical": "{capacity}%<span color='${base08}' > {icon}</span>",
            "format-warning": "{capacity}%<span color='${base0A}' > {icon}</span>",
            "format-full": "{capacity}%<span color='${base0B}' > {icon}</span>",
            "format-charging": "{capacity}%<span color='${base0B}' > {icon} </span>",
            "format-charging-warning": "{capacity}%<span color='${base0A}' > {icon} </span>",
            "format-charging-critical": "{capacity}%<span color='${base08}' > {icon} </span>",
            "format-plugged": "{capacity}%<span color='${base0B}' > {icon} </span>",
            "format-alt": "<span color='${base0B}' > {icon} </span>{time}",
            "tooltip": false
          },
          "tray": { "spacing": 10 },
          "custom/power": {
            "format": "⏻ ",
            "on-click": "wlogout"
          }
        }
      ]   '');

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
