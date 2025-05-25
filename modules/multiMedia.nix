{
  nix.global = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        # utils
        p7zip
        v4l-utils
      ];
      programs = {
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
        texlive.combined.scheme-full
        # apps
        tenacity
        gimp
        picard
      ];
      programs.zathura.enable = true;
    })
  ];
}
