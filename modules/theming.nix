{
  pkgs,
  def,
  ...
}: {
  stylix = {
    enable = true;
    autoEnable = true;
    homeManagerIntegration.autoImport = true;
    image = def.wallpaper;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/classic-dark.yaml";

    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;

    fonts = {
      monospace.package = pkgs.nerd-fonts.jetbrains-mono;
      monospace.name = "JetBrainsMono Nerd Font Mono";
      sansSerif.package = pkgs.noto-fonts-cjk-sans;
      sansSerif.name = "Noto Sans CJK";
      serif.package = pkgs.noto-fonts-cjk-serif;
      serif.name = "Noto Serif CJK";

      sizes = {
        applications = 12;
        terminal = 12;
        desktop = 11;
        popups = 12;
      };
    };
  };

  home-manager.sharedModules = [
    {
      stylix = {
        iconTheme = {
          enable = true;
          package = pkgs.papirus-icon-theme;
          dark = "Papirus-Dark";
        };
        targets = {
          nixvim.transparentBackground.main = true;
          waybar.enable = false;
        };
      };
      gtk = {
        enable = true;
        gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
        gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
      };
      qt = {
        enable = true;
        style.name = "adwaita-dark";
        platformTheme.name = "gtk3";
      };
      programs.oh-my-posh = {
        enable = true;
        settings = builtins.fromJSON (builtins.unsafeDiscardStringContext ''
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
      };
    }
  ];
}
