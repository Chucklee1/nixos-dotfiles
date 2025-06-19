{
  nix.global = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # dependancies
        libnotify
        libsecret
        # utils
        p7zip
        v4l-utils
        ripgrep
        pciutils
        wget
        git
        curl
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
  nix.desktop = [
    # dev work
    ({pkgs, ...}: {
      programs.nix-ld = {
        enable = true;
        libraries = with pkgs; [
          jq
          unzip
          python313
          python313Packages.pip
        ];
      };
    })
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
  home.global = [
    ({
      pkgs,
      nixvim,
      ...
    }: {
      home.packages = with pkgs; [
        # media handling
        file-roller
        imagemagick
        epub-thumbnailer
        ffmpegthumbnailer
        # audio
        ffmpeg-full
        pavucontrol
        mpv
        rmpc
        # dev tools
        openai
        rclone
        python3
        gnumake
        gcc
        gdb # GNU Project debugger
        # apps
        tenacity
        gimp
        picard
        # apps
        qbittorrent
        muse-sounds-manager
        logisim-evolution
        musescore
        nixvim
      ];
      # programs
      programs = {
        btop.enable = true;
        direnv.enable = true;
        librewolf.enable = true;
        vesktop.enable = true;
        yazi.enable = true;
        zathura.enable = true;
      };
      services.mpd = let
        root = "/media/goat/BLUE_SATA/home/server";
      in {
        enable = true;
        dataDir = "${root}/mpd";
        musicDirectory = "${root}/Media/Music";
        network.listenAddress = "any";
        extraConfig = ''
          audio_output {
            type "pipewire"
            name "MPDOUT"
          }
        '';
      };
    })
  ];
  # rip-off rivertuner
  home.desktop = [{programs.mangohud.enable = true;}];
}
