{nixvim, ...}: {
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
  home.global = [
    ({
      config,
      pkgs,
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
        mpvc
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
        librewolf.enable = true;
        vesktop.enable = true;
        yazi.enable = true;
        zathura.enable = true;
      };
      services.mpd = let
        musicDir = "${config.home.homeDirectory}/server/Media/Music";
        dataDir = "${config.home.homeDirectory}/server/mpd";
      in {
        inherit dataDir;
        enable = true;
        musicDirectory = musicDir;
        network.listenAddress = "any";
      };
    })
  ];
}
