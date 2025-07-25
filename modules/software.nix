{inputs, ...}: let
  gaming.nix = {pkgs, ...}: {
    environment.systemPackages = with pkgs; [
      # emulation
      cemu
      joycond
      joycond-cemuhook
      ryubing
      # wine
      zenity
      wine
      wineWowPackages.stagingFull
      winetricks
      # games
      osu-lazer-bin
      prismlauncher
      openmw
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

    # roblox
    services.flatpak.enable = true;
    systemd.services.flatpak-repo = {
      nable = false;
      wantedBy = ["multi-user.target"];
      path = [pkgs.flatpak];
      script = ''flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo '';
    };
  };
  gaming.home = {programs.mangohud.enable = true;};

  nixvim.nix = {nixpkgs.overlays = [inputs.nix-vim.overlays.default];};
  nixvim.home = {pkgs, ...}: {home.packages = [pkgs.nixvim.full];};

  linux.nix = {pkgs, ...}: {
    environment.systemPackages = with pkgs; [udisks mpv pavucontrol];
    programs = {
      nix-ld.enable = true;
      dconf.enable = true;
    };
  };
  linux.home = {pkgs, ...}: {
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
    programs.librewolf.enable = true;
    programs.keepassxc.enable = false;
    programs.vesktop.enable = false;
  };
  wayland.nix = {pkgs, ...}: {
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
  wayland.home = {pkgs, ...}: {
    programs.swaylock.enable = true;
    programs.swaylock.package = pkgs.swaylock-effects;
    programs.waybar.enable = true;
    programs.waybar.systemd.enable = true;
  };
in {
  global = {
    nix = [
      nixvim.nix
      ({pkgs, ...}: {
        environment.systemPackages = with pkgs; [
          curl
          gcc
          gdb # GNU debugger
          ffmpeg-full
          imagemagick
          python3
        ];
      })
    ];
    home = [
      nixvim.home
      ({pkgs, ...}: {
        home.packages = [pkgs.rmpc];
        # programs
        programs = {
          bash.enable = true;
          btop.enable = true;
          direnv.enable = true;
          git.enable = true;
          kitty.enable = true;
          oh-my-posh.enable = true;
          yazi.enable = true;
          zathura.enable = true;
          # find help
          fzf.enable = true;
          zoxide = {
            enable = true;
            options = ["--cmd cd"];
          };
        };
      })
    ];
  };

  desktop.nix = [linux.nix wayland.nix];
  desktop.home = [linux.home wayland.home];

  laptop.nix = [linux.nix wayland.nix];
  laptop.home = [linux.home wayland.home];

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
        casks = ["kitty" "ghostty" "hammerspoon"];
      };
    }
    # rice
    {
      services.jankyborders.enable = true;
      services.sketchybar.enable = true;
    }
  ];
}
