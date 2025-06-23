{
  nix.global = [
    ({
      pkgs,
      machine,
      ...
    }: let
      onlyLinux = opt: opt2: (
        if machine == "macbook"
        then opt2
        else opt
      );
    in {
      environment.systemPackages = with pkgs;
        [
          # finding
          ripgrep
          fzf
          pciutils
          zoxide
          # media
          ffmpeg-full
          imagemagick
          mpv
          # archive management
          p7zip
          unzip
          unrar
          # misc tools
          curl
          gcc
          gdb # GNU Project debugger
          gnumake
          python3
          rclone
        ]
        ++ (onlyLinux [
          pavucontrol
        ] []);

      # programs
      programs = onlyLinux {
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
      } {};
    })
  ];
  nix.desktop = [
    # dev work
    {
      programs.nix-ld = {
        enable = true;
      };
    }
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
      machine,
      ...
    }: let
      onlyLinux = opt: opt2: (
        if machine == "macbook"
        then opt2
        else opt
      );
    in {
      home.packages = with pkgs;
        [nixvim]
        ++ (onlyLinux [
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
        // (onlyLinux {
          librewolf.enable = true;
          rmpc.enable = true;
          vesktop.enable = true;
        } {});
    })
  ];
  # rip-off rivertuner
  home.desktop = [{programs.mangohud.enable = true;}];
}
