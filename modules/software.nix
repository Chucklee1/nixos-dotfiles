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
      home = {

      # latexrc
        file.".latexmkrc".text = #sh 
      '' add_cus_dep( 'acn', 'acr', 0, 'makeglossaries' );
add_cus_dep( 'glo', 'gls', 0, 'makeglossaries' );
$clean_ext .= " acr acn alg glo gls glg";
sub makeglossaries {
     my ($base_name, $path) = fileparse( $_[0] );
     my @args = ( "-d", $path, $base_name );
     if ($silent) { unshift @args, "-q"; }
     return system "makeglossaries", @args;
}'';
      packages = with pkgs; [
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
        # apps
        qbittorrent
        muse-sounds-manager
        (ungoogled-chromium.override {enableWideVine = true;})
        logisim-evolution
        musescore
        nixvim
      ];};
      programs.zathura.enable = true;
    })
  ];
}
