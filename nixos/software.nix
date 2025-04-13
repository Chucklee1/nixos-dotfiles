{
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
            thunar-volman
          ];
        };
      };
    })
  ];

  home.global = [
    ({pkgs, ...}: {
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
        spotdl
        picard
        feishin
        qbittorrent
        musescore
        libreoffice
        logisim-evolution
      ];

      programs = {
        # diagnostics
        btop.enable = true;
        mangohud.enable = true;
        # browser
        chromium = {
          enable = true;
          package = pkgs.ungoogled-chromium;
        };
      };
    })
  ];

  nix.desktop = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        osu-lazer-bin
        prismlauncher
      ];
    })
    {
      programs.steam = {
        enable = true;
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
      };
      environment.variables.STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools..d";
    }
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
  ];
}
