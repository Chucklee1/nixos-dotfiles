{
  nix.global = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # dependancies
        libnotify
        libsecret
        # web
        wget
        git
        curl
        # previewing
        epub-thumbnailer
        ffmpegthumbnailer
        poppler
        # finding
        fd
        rg
        jq
        fzf
        pciutils
        zoxide
        # media
        ffmpeg-full
        v4l-utils
        imagemagick
        mpv
        pavucontrol
        p7zip
        # dev tools
        gcc
        gdb # GNU Project debugger
        gnumake
        openai
        python3
        rclone
      ];

      # programs
      programs = {
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
      lib,
      pkgs,
      nixvim,
      ...
    }: {
      home.packages = with pkgs; [
        gimp
        logisim-evolution
        musescore
        muse-sounds-manager
        nixvim
        picard
        qbittorrent
        tenacity
      ];
      # programs
      programs = lib.genAttrs (n: {enable = true;}) [
        "bash"
        "btop"
        "direnv"
        "git"
        "kitty"
        "librewolf"
        "oh-my-posh"
        "rmpc"
        "vesktop"
        "yazi"
        "zathura"
      ];
    })
  ];
  # rip-off rivertuner
  home.desktop = [{programs.mangohud.enable = true;}];
}
