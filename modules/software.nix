{inputs, ...}: {
  nix.global = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # dependancies
        libnotify
        libsecret
        # cli
        ripgrep
        pciutils
        # web/net
        wget
        git
        curl
        # media/files
        file-roller
        p7zip
        mpv
        ffmpeg
        pavucontrol
        v4l-utils
      ];

      # programs
      programs = {
        dconf.enable = true;
        xfconf.enable = true;
        thunar = {
          enable = true;
          plugins = with pkgs.xfce; [
            thunar-archive-plugin
            thunar-media-tags-plugin
            thunar-volman
          ];
        };
      };
    })
  ];
  home.global = [
    ({
      lib,
      pkgs,
      ...
    }: {
      home.packages = with pkgs; [
        # dev tools
        openai
        python3
        gnumake
        gdb
        gcc
        # apps
        tenacity
        gimp
        picard
        qbittorrent
        musescore
        muse-sounds-manager
        logisim-evolution
      ];

      stylix.targets.firefox.profileNames = ["default"];
      programs = {
        # diagnostics
        btop.enable = true;
        mangohud.enable = true;
        #git
        git = {
          enable = true;
          userEmail = "kermitthefrog@kakao.com";
          userName = "Chucklee1";
        };

        # browser
        firefox = {
          enable = true;
          policies = {
            enableTrackingProtection = {
              Value = true;
              Locked = true;
              Cryptomining = true;
              Fingerprinting = true;
            };
            DisableTelemetry = true;
            DisablePocket = true;
            DisableFirefoxStudies = true;
            DisableFirefoxAccounts = true;
            DisableFirefoxScreenshots = true;
            DisableSafeMode = true;
          };
          profiles.default = {
            name = "default";
            settings = {
              "browser.startup.homepage" = "";
              "ui.prefersReducedMotion" = "true";
            };
            userChrome = ''${builtins.readFile "${inputs.personal}/chrome.css}"}'';
            bookmarks = {
              force = true;
              configFile = ''${builtins.readFile "${inputs.personal}/bookmarks.html}"}'';
            };

            search = {
              default = "ddg";
              engines = {
                nix-packages = {
                  name = "Nix Packages";
                  urls = [
                    {
                      template = "https://search.nixos.org/packages";
                      params = [
                        {
                          name = "type";
                          value = "packages";
                        }
                        {
                          name = "query";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];

                  icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                  definedAliases = ["@np"];
                };
                my-nixos = {
                  name = "my nixos";
                  urls = [
                    {
                      template = "https://mynixos.com/search";
                      params = [
                        {
                          name = "q";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = ["@mp"];
                };
              };
            };
          };
        };

        # shell
        kitty = {
          enable = true;
          settings = {
            confirm_os_window_close = 0;
            hide_window_decorations = true;
            tab_bar_edge = "top";
            tab_bar_style = lib.mkForce "slant";
          };
        };

        bash = {
          enable = true;
          shellAliases = let
            flake = "--impure --show-trace --flake $HOME/nixos-dotfiles";
          in {
            # nix - system installation
            disko-generate = ''sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko /tmp/disko.nix'';
            show-hardware = "sudo nixos-generate-config ${flake}";
            install-mnt = "sudo nixos-install --root /mnt ${flake}";
            # nix - general
            cg = "nix-collect-garbage";
            update-flake = "nix flake update --flake $HOME/nixos-dotfiles";
            rebuild-desktop = "rm -rf $HOME/.mozilla && sudo nixos-rebuild switch ${flake}#desktop";
            rebuild-macbook = "sudo nixos-rebuild switch  ${flake}#macbook";
            # tools - git
            clone-flake = ''
              if [ -e /home/goat/.ssh/id_ed25519.pub ]; then
                echo "found ssh key, cloning with ssh..."
                git clone git@github.com:Chucklee1/nixos-dotfiles
              else
                echo "no ssh key found, trying tls..."
                git clone https://github.com/Chucklee1/nixos-dotfiles
              fi
            '';
            # tools - ffmpeg
            wav-to-flac = ''
              for i in *.wav; do ffmpeg -i "$i" -c:a flac "$i%.*.flac"; done
              for i in *.wav; do rm "$i"; done
            '';
          };
        };
        oh-my-posh = {
          enable = true;
          enableBashIntegration = true;
          useTheme = "pure";
        };
      };
    })
  ];

  nix.desktop = [
    # games
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        osu-lazer-bin
        prismlauncher
      ];
      programs.steam = {
        enable = true;
        protontricks.enable = true;
        gamescopeSession.enable = true;
        extraCompatPackages = [pkgs.proton-ge-bin];
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
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
      ];
    })
  ];
}
