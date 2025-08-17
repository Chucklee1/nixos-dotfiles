{
  global.nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        calc
        curl
        ffmpeg-full
        gcc
        gdb # GNU debugger
        imagemagick
        python3
      ];
    })
  ];

  linux.nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [udisks mpv pavucontrol];
      programs.dconf.enable = true;
    })
  ];
  linux.home = [{programs.zathura.enable = true;}];

  metal.nix = [{programs.nix-ld.enable = true;}];
  metal.home = [
    ({pkgs, ...}: {
      home.packages = with pkgs; [
        calibre
        krita
        logisim-evolution
        musescore
        muse-sounds-manager
        picard
        qbittorrent
        tenacity
      ];
    })
  ];

  macbook.nix = [
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
        casks = ["kitty" "skim"];
      };
    }
  ];
}
