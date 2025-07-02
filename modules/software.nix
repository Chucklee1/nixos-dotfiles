{inputs, ...}: {
  nix.global = [
    ({
      pkgs,
      ifSys,
      ...
    }: {
      environment.systemPackages = with pkgs;
        [
          # info helpers
          ripgrep
          fzf
          pciutils
          zoxide
          # media
          ffmpeg-full
          imagemagick
          mediainfo
          mpv
          # file-management
          p7zip
          rclone
          rar
          trash-cli
          unzip
          unrar
          udisks
          zip
          # dev
          gcc
          gdb # GNU debugger
          gnumake
          python3
          curl
        ]
        ++ (ifSys.linux [pavucontrol] []);

      # programs
      programs =
        ifSys.darwin {
          bash = {
            completion.enable = true;
            enable = true;
            interactiveShellInit = "";
          };
        } {
          nix-ld.enable = true;
          dconf.enable = true;
          xfconf.enable = false;
          thunar = {
            enable = false;
            plugins = with pkgs.xfce; [
              thunar-archive-plugin
              thunar-media-tags-plugin
              thunar-volman
            ];
          };
        };
    })
  ];
  nix.desktop = [
    # dev work
    # games
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        osu-lazer-bin
        prismlauncher
        ryubing
        cemu
        joycond
        joycond-cemuhook
      ];
      programs.gamemode = {
        enable = true;
        settings.general.desiredgov = "performance";
        settings.general.renice = 10;
      };
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
    # roblox
    ({pkgs, ...}: {
      services.flatpak.enable = true;
      systemd.services.flatpak-repo = {
        wantedBy = ["multi-user.target"];
        path = [pkgs.flatpak];
        script = ''
          flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
        '';
      };
    })
  ];
  home.global = [
    ({
      pkgs,
      ifSys,
      ...
    }: {
      home.packages = with pkgs;
        [inputs.nix-vim.packages.${system}.full]
        ++ (ifSys.linux [
          gimp
          logisim-evolution
          musescore
          muse-sounds-manager
          picard
          qbittorrent
          tenacity
          kitty
        ] []);

      # programs
      programs =
        {
          bash.enable = true;
          btop.enable = true;
          direnv.enable = true;
          git.enable = true;
          oh-my-posh.enable = true;
          yazi.enable = true;
          zathura.enable = true;
        }
        // (ifSys.darwin {} {
          librewolf.enable = true;
          rmpc.enable = true;
          vesktop.enable = true;
        });
    })
  ];
  nix.macbook = [
    {
      # homebrew
      homebrew = {
        enable = true;
        onActivation = {
          autoUpdate = true;
          upgrade = true;
          cleanup = "zap";
        };
        caskArgs.no_quarantine = true;
        #brews = [ ];
        casks = [
          "dmenu-mac"
          "kitty"
          "librewolf"
          "prismlauncher"
        ];
      };
    }
  ];
  # rip-off rivertuner
  home.desktop = [{programs.mangohud.enable = true;}];
}
