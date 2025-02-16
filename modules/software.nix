{
  nix.global = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # dependancies
        libnotify
        libsecret
        # development
        openai
        python3
        gnumake
        gdb
        gcc
        gimp
        # cli
        ripgrep
        pciutils
        btop
        ncdu
        # web/net
        wget
        git
        curl
        # media/files
        file-roller
        p7zip
        pavucontrol
        v4l-utils
        # apps
        spotify
        prismlauncher
      ];

      # programs
      programs = {
        dconf.enable = true;
        xfconf.enable = true;
        firefox.enable = true;
        thunar = {
          enable = true;
          plugins = with pkgs.xfce; [
            thunar-archive-plugin
            thunar-volman
          ];
        };
      };
    })

    # wine
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        zenity
        samba
        wine
        wineWowPackages.stagingFull
        winetricks
        protonup-qt
        protontricks
      ];
    })

    # steam
    {
      programs.steam = {
        enable = true;
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
      };
      environment.variables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools..d";
    }
  ];

  home.global = [
    ({lib, config, ...}: {
      programs = {
        # git
        git = {
          enable = true;
          userEmail = "cooperkang4@gmail.com";
          userName = "Chucklee1 - remote";
        };
        # terminal emulator
        kitty = {
          enable = true;
          settings = {
            confirm_os_window_close = 0;
            hide_window_decorations = true;
            tab_bar_style = lib.mkForce "slant";
          };
        };
        # shell
        bash = {
          enable = true;
          shellAliases = {
            cg = "nix-collect-garbage";
            flake = "$HOME/nixos-dotfiles/flake.nix";
            update-flake = "nix flake update $HOME/nixos-dotfiles";
            update-laptop = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#laptop";
            update-desktop = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#desktop";
          };
        };
        oh-my-posh = {
          enable = true;
          enableBashIntegration = true;
          settings = let
            colorWithHash = config.lib.stylix.colors.withHashtag;
          in
            with colorWithHash;
              builtins.fromJSON (builtins.unsafeDiscardStringContext
                # json
                ''
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
        };
      };
    })
  ];

  nix.desktop = [
    ({pkgs, ...}: {environment.systemPackages = [pkgs.osu-lazer-bin];})
  ];
}
