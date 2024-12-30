{
  pkgs,
  lib,
  inputs,
  def,
  ...
}: {
  imports = [./theming.mod.nix];

  # -----------------------------------------------------------
  # boot
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
    hostName = "${def.username}";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };

  i18n.defaultLocale = "en_CA.UTF-8";
  time.timeZone = "America/Vancouver";
  console = {
    earlySetup = true;
    keyMap = def.layout;
  };

  # -----------------------------------------------------------
  # nix options
  # -----------------------------------------------------------
  nixpkgs = {
    hostPlatform = lib.mkDefault "${def.system}";
    config.allowUnfree = true;
    overlays = [inputs.niri.overlays.niri];
  };
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
  };

  # -----------------------------------------------------------
  # user & home manager
  # -----------------------------------------------------------
  users.users.${def.username} = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager" "libvirtd"];
  };
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs def;};
    users.${def.username}.home = {
      stateVersion = "24.05"; # DO NOT CHANGE
      username = "${def.username}";
      homeDirectory = "/home/${def.username}";
    };
    sharedModules = [
      ./niri.home.nix
      ./nixvim.home.nix
      ./shelli.home.nix
      ./waybar.home.nix
      {
        home.sessionVariables = {
          NIXOS_OZONE_WL = "1";
          MOZ_ENABLE_WAYLAND = "1";
          SDL_VIDEODRIVER = "wayland";
          GDK_BACKEND = "wayland,x11";
          QT_QPA_PLATFORM = "wayland;xcb";
          QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
          QT_AUTO_SCREEN_SCALE_FACTOR = "1";
        };

        programs = {
          fuzzel.enable = true;
          wlogout.enable = true;
          emacs.enable = true;
        };
        # most wm services
        services = {
          dunst.enable = true;
          gnome-keyring.enable = true;
        };
      }
    ];
  };

  # -----------------------------------------------------------
  # system packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools/deps
    gcc
    vulkan-tools
    ffmpeg
    v4l-utils
    libnotify
    libsecret
    # language QOL
    alejandra
    nixd
    asm-lsp
    # cli
    killall
    ripgrep
    pciutils
    btop
    ncdu
    # web/net
    wget
    git
    curl
    # wayland
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland
    # window manager utils
    wev
    brightnessctl
    xclip
    wl-clipboard
    cliphist
    swaybg
    wlsunset
    networkmanagerapplet
    # xwayland
    xwayland
    xwayland-run
    xsel
    xclip
    # media
    mpv
    imv
    pavucontrol
    # apps/games
    firefox
    #openmw
    # misc
    nerd-fonts.symbols-only
    file-roller
    p7zip
  ];

  # -----------------------------------------------------------
  # system programs
  # -----------------------------------------------------------
  programs = {
    niri.enable = true;
    dconf.enable = true;
    xfconf.enable = true; # for thunar config
    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
      ];
    };
  };

  # -----------------------------------------------------------
  # infastrcuture
  # -----------------------------------------------------------

  # security and portals
  security = {
    polkit.enable = true;
    rtkit.enable = true; # for sound
  };
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };

  # hardware
  hardware = {
    graphics.enable = true; # renamed opengl to graphics as of 24.11
    graphics.enable32Bit = true;
    bluetooth.enable = true;
    bluetooth.powerOnBoot = true;
  };

  # services
  services = {
    xserver = {
      enable = true;
      xkb.layout = "us";
      xkb.variant = "";
    };
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    blueman.enable = true;
    fstrim.enable = true;
    displayManager.ly.enable = true;
    libinput.enable = true;
    printing.enable = true;
    openssh.enable = true;
    # for thunar
    tumbler.enable = true;
    gvfs.enable = true;
  };
}
