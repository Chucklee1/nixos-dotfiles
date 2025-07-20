let
  linux = {
    nix = {pkgs, ...}: {
      environment.systemPackages = with pkgs; [udisks mpv pavucontrol];
      programs = {
        nix-ld.enable = true;
        dconf.enable = true;
      };
    };
    home = {pkgs, ...}: {
      home.packages = with pkgs; [
        krita
        logisim-evolution
        musescore
        muse-sounds-manager
        picard
        qbittorrent
        tenacity
        kitty
      ];
      programs.librewolf.enable = true;
      programs.vesktop.enable = true;
    };
  };
  wayland = {
    nix = {pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        egl-wayland
        qt5.qtwayland
        qt6.qtwayland
        brightnessctl
        wev
        wmenu
        swaynotificationcenter
        xwayland
        xwayland-run
        wl-color-picker
        wl-clipboard
      ];

      # polkit n portals
      security.polkit.enable = true;
      xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-gnome
        pkgs.xdg-desktop-portal-gtk
      ];
      xdg.portal.config.common.default = "gnome";
    };
    home = {pkgs, ...}: {
      programs.swaylock.enable = true;
      programs.swaylock.package = pkgs.swaylock-effects;
      programs.waybar.enable = true;
      programs.waybar.systemd.enable = true;
    };
  };
in {
  global = {
    nix = [
      ({pkgs, ...}: {
        environment.systemPackages = with pkgs; [
          curl
          gcc
          gdb # GNU debugger
          gnumake
          ffmpeg-full
          imagemagick
          python3
        ];
      })
    ];
    home = [
      ({pkgs, ...}: {
        home.packages = [pkgs.rmpc];
        # programs
        programs = {
          bash.enable = true;
          btop.enable = true;
          direnv.enable = true;
          git.enable = true;
          oh-my-posh.enable = true;
          yazi.enable = true;
          zathura.enable = true;
        };
      })
    ];
  };

  desktop = {
    nix = [
      linux.nix
      wayland.nix
      {programs.river.enable = true;}
      # games
      ({pkgs, ...}: {
        environment.systemPackages = with pkgs; [
          cemu
          joycond
          joycond-cemuhook
          openmw
          osu-lazer-bin
          #prismlauncher
          ryubing
        ];
        programs.gamemode = {
          enable = true;
          settings.general.desiredgov = "performance";
          settings.general.renice = 10;
        };
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
      # roblox
      ({pkgs, ...}: {
        services.flatpak.enable = true;
        systemd.services.flatpak-repo = {
          wantedBy = ["multi-user.target"];
          path = [pkgs.flatpak];
          script = ''
            flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
          '';
        };
      })
    ];
    home = [
      linux.home
      wayland.home
      {programs.mangohud.enable = true;}
    ];
  };

  laptop = {
    nix = [linux.nix wayland.nix];
    home = [linux.home wayland.home];
  };

  macbook.nix = [
    # bash
    {
      programs.bash = {
        completion.enable = true;
        enable = true;
        interactiveShellInit = "";
      };
    }
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
        casks = [
          "kitty"
          "librewolf"
          "hammerspoon"
        ];
      };
    }
    # rice
    {
      services.jankyborders.enable = true;
      services.sketchybar.enable = true;
    }
  ];
}
