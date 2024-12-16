{
  lib,
  system,
  inputs,
  defaults,
  ...
}: {
  # -----------------------------------------------------------
  # boot loader
  # -----------------------------------------------------------
  boot.loader = {
    efi.canTouchEfiVariables = true;
    grub = {
      enable = true;
      efiSupport = true;
      device = "nodev";
    };
    grub2-theme = {
      enable = true;
      theme = "stylish";
      footer = true;
    };
  };

  # -----------------------------------------------------------
  # system options
  # -----------------------------------------------------------

  networking = {
    hostName = "${defaults.username}";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };

  system.stateVersion = "24.05"; # DO NOT CHANGE
  time.timeZone = "${defaults.timeZone}";
  i18n.defaultLocale = "${defaults.locale}";
  console = {
    earlySetup = true;
    keyMap = "us";
  };

  # -----------------------------------------------------------
  # nix options
  # -----------------------------------------------------------
  nixpkgs = {
    hostPlatform = lib.mkDefault "${system}";
    config.allowUnfree = true;
  };
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
  };

  # -----------------------------------------------------------
  # system user declaration
  # -----------------------------------------------------------
  users.users.${defaults.username} = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };

  # -----------------------------------------------------------
  # home manager
  # -----------------------------------------------------------
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs;};
    users.${defaults.username}.home = {
      stateVersion = "24.05"; # DO NOT CHANGE
      username = "${defaults.username}";
      homeDirectory = "/home/${defaults.username}";
    };
    sharedModules = [
      {
        home.packages = [
          pkgs.papirus-icon-theme
          (pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
        ];
        gtk = {
          iconTheme = {
            name = "Papirus-Dark";
            package = pkgs.papirus-icon-theme;
          };
          gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
          gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
        };
        qt = {
          enable = true;
          style.name = "adwaita-dark";
          platformTheme.name = "gtk3";
        };
      }
    ];
  };

  # -----------------------------------------------------------
  # system packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools
    gcc
    alejandra
    nixd
    asm-lsp
    # cli
    killall
    ripgrep
    pciutils
    zenity
    btop
    ncdu
    # web/net
    wget
    git
    curl
    # compresssion, archiving, & filed
    unrar
    unzip
    p7zip
    file-roller
    tree
    isoimagewriter
    # wayland
    wayland-utils
    wayland-scanner
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland
    # clipboard
    wl-clipboard
    cliphist
    # media
    mpv
    imv
    ffmpeg
    v4l-utils
    # hardware I/O tools
    wev
    pavucontrol
    # libs
    libnotify
    libsecret
    # misc
    neofetch
    sl
    cowsay
    firefox
  ];

  # -----------------------------------------------------------
  # infastrcuture
  # -----------------------------------------------------------

  # graphics
  hardware.graphics.enable = true; # renamed opengl to graphics as of 24.11
  hardware.graphics.enable32Bit = true;

  # bluetooth
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  serivces.blueman.enable = true;

  # sound
  security.rtkit.enable = true; # for sound
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # the rest
  services = {
    fstrim.enable = true;
    displayManager.ly.enable = true;
    libinput.enable = true;
    printing.enable = true;
    openssh.enable = true;
  };

  security.polkit.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };
}
