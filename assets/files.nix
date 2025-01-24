{
  config,
  lib,
  ...
}: let
  colorWithHash = lib.mapAttrs (name: color: "#${color}") config.lib.stylix.colors;
in
  with colorWithHash; {
    waybarConfig = builtins.fromJSON "${builtins.readFile ./waybar/config.json}";

    waybarStyle = ''
      * {
        font-family: "JetBrainsMono nerd font";
        font-size: 12px;
      }
      window#waybar {
        color: ${base07};
        transition-duration: .5s;
      }

      #battery,
      #cpu,
      #memory,
      #disk,
      #backlight,
      #network,
      #pulseaudio,
      #tray,
      #idle_inhibitor { padding: 0 10px; }
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
