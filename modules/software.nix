{inputs, ...}: {
  global.nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        age
        calc
        curl
        ffmpeg-full
        gcc
        gdb # GNU debugger
        imagemagick
        just
        lua
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

  desktop.nix = [
    {programs.nix-ld.enable = true;}
    ({pkgs, ...}: {
      services.flatpak.enable = true;
      systemd.services.flatpak-repo = {
        wantedBy = [ "multi-user.target" ];
        path = [ pkgs.flatpak ];
        script = ''
          flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
        '';
      };
    })
  ];
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
    inputs.nix-homebrew.darwinModules.nix-homebrew
    ({user, ...}: {
      nix-homebrew = {
        inherit user; # User owning the Homebrew prefix
        enable = true;
        autoMigrate = true;
        enableRosetta = true;

        # Declarative tap management
        taps = {
          "homebrew/homebrew-core" = inputs.homebrew-core;
          "homebrew/homebrew-cask" = inputs.homebrew-cask;
        };
        # With mutableTaps disabled, taps can no longer be added imperatively with `brew tap`.
        mutableTaps = false;
      };
    })
    ({config, ...}: {
      homebrew = {
        enable = true;
        onActivation = {
          autoUpdate = true;
          upgrade = true;
          cleanup = "zap";
        };

        taps = builtins.attrNames config.nix-homebrew.taps; #Align homebrew taps config with nix-homebrew

        caskArgs.no_quarantine = true;
        casks = [
          "dmenu-mac"
          "hammerspoon"
          "krita"
          "tailscale"
          "utm"
          "xquartz"
        ];
      };
    })
  ];
}
