{config, ...}: {
  custom-darkblood = let
    text = config.scheme.withHashtag.base07;
    base = config.scheme.withHashtag.base08;
  in
    builtins.fromJSON (builtins.unsafeDiscardStringContext ''
      {
        "$schema": "https://raw.githubusercontent.com/JanDeDobbeleer/oh-my-posh/main/themes/schema.json",
        "blocks": [
          {
            "alignment": "left",
            "segments": [
              {
                "foreground": "${text}",
                "style": "plain",
                "template": "<${base}>\u250f[</>{{ .UserName }}<${base}>]</>",
                "type": "session"
              },
              {
                "foreground": "${text}",
                "style": "plain",
                "template": "<${base}>[</>{{ .HEAD }}<${base}>]</>",
                "type": "git"
              },
              {
                "foreground": "${text}",
                "style": "plain",
                "template": "<${base}>[</>\uf0e7<${base}>]</>",
                "type": "root"
              },
              {
                "foreground": "${text}",
                "style": "plain",
                "template": "<${base}>[x</>{{ reason .Code }}<${base}>]</>",
                "type": "status"
              }
            ],
            "type": "prompt"
          },
          {
            "alignment": "left",
            "newline": true,
            "segments": [
              {
                "foreground": "${text}",
                "properties": {
                  "style": "full"
                },
                "style": "plain",
                "template": "<${base}>\u2516[</>{{ .Path }}<${base}>]></>",
                "type": "path"
              }
            ],
            "type": "prompt"
          }
        ],
        "final_space": true,
        "version": 3
      }
    '');
  custom-pure = builtins.fromJSON (builtins.unsafeDiscardStringContext ''
    {
      "$schema": "https://raw.githubusercontent.com/JanDeDobbeleer/oh-my-posh/main/themes/schema.json",
      "blocks": [
        {
          "alignment": "left",
          "segments": [
            {
              "foreground": "#BF616A",
              "style": "plain",
              "template": "{{ .UserName }} ",
              "type": "session"
            },
            {
              "foreground": "#81A1C1",
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
              "foreground": "#6C6C6C",
              "properties": {
                "branch_ahead_icon": "<#88C0D0>\u21e1 </>",
                "branch_behind_icon": "<#88C0D0>\u21e3 </>",
                "branch_icon": "",
                "fetch_stash_count": true,
                "fetch_status": true,
                "fetch_upstream_icon": true,
                "github_icon": ""
              },
              "style": "plain",
              "template": "{{ .UpstreamIcon }}{{ .HEAD }}{{if .BranchStatus }} {{ .BranchStatus }}{{ end }}{{ if .Working.Changed }}<#FFAFD7>*</>{{ .Working.String }}{{ end }}{{ if and (.Working.Changed) (.Staging.Changed) }} |{{ end }}{{ if .Staging.Changed }} \uf046 {{ .Staging.String }}{{ end }}{{ if gt .StashCount 0 }} \ueb4b {{ .StashCount }}{{ end }} ",
              "type": "git"
            }
          ],
          "type": "prompt"
        },
        {
          "alignment": "left",
          "segments": [
            {
              "foreground": "#A3BE8C",
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
              "foreground": "#B48EAD",
              "foreground_templates": [
                "{{ if gt .Code 0 }}#BF616A{{ end }}"
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
        "foreground": "#B48EAD",
        "foreground_templates": [
          "{{ if gt .Code 0 }}#BF616A{{ end }}"
        ],
        "template": "\u276f "
      },
      "version": 3
    }
  '');
}
