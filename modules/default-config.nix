{
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ./GPU/nvidia.nix
  ];

  # Bootloader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    plymouth.enable = true;
  };

  # tty settings
  console.font = "Lat2-Terminus16";
  console.useXkbConfig = true;

  # nix
  nixpkgs.config.allowUnfree = true;
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
  };

  # idk
  networking.networkmanager.enable = true;
  time.timeZone = "America/Vancouver";
  i18n.defaultLocale = "en_CA.UTF-8";

  # system user config
  users.users = {
    goat = {
      isNormalUser = true;
      extraGroups = ["wheel" "networkmanager"];
    };
    caprine = {
      isNormalUser = true;
      extraGroups = ["wheel" "networkmanager"];
    };
  };
  # home manager user config
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "backup";
    users = {
      goat = {
        imports = [./home/default-home.nix];
        home.username = "goat";
        home.homeDirectory = "/home/goat";
      };
      caprine = {
        imports = [./home/default-home.nix];
        home.username = "caprine";
        home.homeDirectory = "/home/caprine";
      };
    };
  };

  # theming
  fonts.packages = [(pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})]; # needed for waybar and misc icons
  stylix = {
    enable = true;
    homeManagerIntegration.autoImport = true;
    image = ../wallpapers/clouds-sunset.jpg;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/tokyo-city-terminal-dark.yaml";
    opacity.terminal = 0.6;
    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;
    fonts = {
      monospace = {
        package = pkgs.nerdfonts.override {fonts = ["JetBrainsMono"];};
        name = "JetBrainsMono Nerd Font Mono";
      };
      sansSerif = {
        package = pkgs.noto-fonts-cjk-sans;
        name = "Noto Sans CJK";
      };
      serif = {
        package = pkgs.noto-fonts-cjk-serif;
        name = "Noto Serif CJK";
      };
      sizes = {
        applications = 12;
        terminal = 12;
        desktop = 11;
        popups = 12;
      };
    };
  };

  # env packages
  environment.systemPackages = with pkgs; [
    # Development Tools
    ripgrep
    alejandra
    nixd
    libgccjit
    rustc

    # Command-Line Utilities
    killall
    pciutils
    sl
    cowsay
    neofetch

    # Web & Networking Utilities
    wget
    git
    curl
    networkmanagerapplet

    # Compression & Archiving
    unrar
    unzip
    file-roller
    tree
    isoimagewriter

    # Wayland & Display Utilities
    wayland
    wayland-protocols
    wayland-utils
    wayland-scanner
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland

    # Clipboard & Clipboard Management
    wl-clipboard
    cliphist
    xclip

    # Security & Authentication
    libsecret
    lxqt.lxqt-policykit

    # Media Tools
    mpv
    imv
    ffmpeg
    v4l-utils

    # Keyboard & Input Tools
    wev
    ydotool
    wtype

    # System Controls
    playerctl
    pavucontrol
    brightnessctl
    wlsunset
  ];

  # programs
  programs = {
    thunar.enable = true;
    thunar.plugins = with pkgs.xfce; [
      thunar-archive-plugin
      thunar-volman
    ];
    seahorse.enable = true; # password app
    niri.enable = true;
  };

  # opengl option, renamed to graphics as of 24.11
  hardware.graphics.enable = true;

  # bluetooth
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.blueman.enable = true;

  # sound
  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # display-manager
  services.displayManager = {
    enable = true;
    ly.enable = true;
    defaultSession = "niri";
  };
  # misc services
  services = {
    printing.enable = true;
    gvfs.enable = true;
    tumbler.enable = true;
    fstrim.enable = true;
    gnome.gnome-keyring.enable = true;
    openssh.enable = true;
  };

  # security
  security = {
    rtkit.enable = true; # sound
    polkit.enable = true; # polkit
    polkit.extraConfig = ''
      polkit.addRule(function(action, subject) {
        if (
          subject.isInGroup("users")
            && (
              action.id == "org.freedesktop.login1.reboot" ||
              action.id == "org.freedesktop.login1.reboot-multiple-sessions" ||
              action.id == "org.freedesktop.login1.power-off" ||
              action.id == "org.freedesktop.login1.power-off-multiple-sessions"
            )
          )
        {
          return polkit.Result.YES;
        }
      })
    '';
    pam.services.swaylock = {
      # locking
      text = ''
        auth include login
      '';
    };
  };

  # xdg portal
  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
    configPackages = [
      pkgs.xdg-desktop-portal
      pkgs.xdg-desktop-portal-gtk
    ];
  };

  # D O  N O T  C H A N G E
  system.stateVersion = "24.05";
}