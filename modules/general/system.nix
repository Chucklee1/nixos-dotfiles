{
  pkgs,
  lib,
  inputs,
  defaults,
  system,
  ...
}: {
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
    hostName = "${defaults.username}";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };

  i18n.defaultLocale = "en_CA.UTF-8";
  time.timeZone = "America/Vancouver";
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
    overlays = [inputs.niri.overlays.niri];
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
    extraGroups = ["wheel" "networkmanager" "libvirtd"];
  };
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs;};
    users.${defaults.username}.home = {
      stateVersion = "24.05"; # DO NOT CHANGE
      username = "${defaults.username}";
      homeDirectory = "/home/${defaults.username}";
    };
  };

  # -----------------------------------------------------------
  # home manager
  # -----------------------------------------------------------
  home-manager.sharedModules = [
    {
      home.sessionVariables = {
        # wayland
        XDG_SESSION_TYPE = "wayland";
        CLUTTER_BACKEND = "wayland";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
        # xwayland compat.
        DISPLAY = ":0";
        SDL_VIDEODRIVER = "x11";
        QT_QPA_PLATFORM = "wayland;xcb";
        GDK_BACKEND = "wayland,x11,*";
      };
      programs = {
        # wm
        waybar.enable = true;
        fuzzel.enable = true;
        wlogout.enable = true;
        # shelli
        git.enable = true;
        lazygit.enable = true;
        tmux.enable = true;
        kitty.enable = true;
        bash.enable = true;
        oh-my-posh.enable = true;
        # editor
        vscode.enable = true;
        nixvim.enable = true;
      };
      # theming
      gtk.enable = true;
      qt.enable = true;
      # most wm services
      services = {
        dunst.enable = true;
        gnome-keyring.enable = true;
      };
    }
  ];

  # -----------------------------------------------------------
  # system packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools/deps
    gcc
    zenity
    vulkan-tools
    ffmpeg
    v4l-utils
    libnotify
    libsecret
    wineWowPackages.stagingFull
    samba # wine security features
    winetricks
    protonup-qt
    protontricks
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
    # hyprland
    hyprland-protocols
    hyprshot
    hypridle
    hyprlock
    hyprsunset
    # window manager utils
    wev
    brightnessctl
    xsel
    wl-clipboard
    cliphist
    swaybg
    wlsunset
    networkmanagerapplet
    lxqt.lxqt-policykit
    # media
    mpv
    imv
    pavucontrol
    # apps/games
    firefox
    openmw
    osu-lazer-bin
    prismlauncher
    # misc
    nerd-fonts.symbols-only
    file-roller
    p7zip
  ];

  # -----------------------------------------------------------
  # system programs
  # -----------------------------------------------------------
  stylix.enable = true;
  programs = {
    # wm
    niri.enable = true;
    hyprland.enable = true;
    # games
    gamemode.enable = true;
    steam.enable = true;
    # virt related
    virt-manager.enable = true;
    dconf.enable = true;
    # file management
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

  # polkit and xdg portal support
  security.polkit.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };

  # graphics
  hardware.graphics.enable = true; # renamed opengl to graphics as of 24.11
  hardware.graphics.enable32Bit = true;

  # bluetooth
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.blueman.enable = true;

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
    # for thunar
    tumbler.enable = true;
    gvfs.enable = true;
  };
}
