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
    ({pkgs, ...}: {
      home.packages = with pkgs; [
        # files
        file-roller
        fontpreview
        epub-thumbnailer
        ffmpegthumbnailer
        # audio
        ffmpeg-full
        pavucontrol
        mpv
        # images
        imagemagick
        # latex
        texpresso
        tectonic-unwrapped
        # apps
        tenacity
        gimp
        picard
        # apps
        qbittorrent
        muse-sounds-manager
        (ungoogled-chromium.override {enableWideVine = true;})
        logisim-evolution
        musescore
        nixvim
      ];
      programs.zathura.enable = true;
    })
  ];
}
