let
  nixLinux = {pkgs, ...}: {
    environment.systemPackages = with pkgs; [udisks mpv pavucontrol];
    programs = {
      nix-ld.enable = true;
      dconf.enable = true;
    };
  };
  nixWayland = {pkgs, ...}: {
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
  homeLinux = {pkgs, ...}: {
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
    programs = {
      librewolf.enable = true;
      vesktop.enable = true;
    };
  };
  homeWayland = {
    programs.swaylock.enable = true;
    programs.waybar.enable = true;
  };
in {
  global.nix = [
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

  desktop.nix = [
    nixLinux
    nixWayland
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

  global.home = [
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
  # rip-off rivertuner
  desktop.home = [
    homeLinux
    homeWayland
    {programs.mangohud.enable = true;}
  ];
}
