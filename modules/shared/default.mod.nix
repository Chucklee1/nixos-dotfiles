{
  pkgs,
  lib,
  inputs,
  def,
  host,
  ...
}: {
  imports = [
    ../hosts/${host}/default.nix
    ./dwm.nix
  ];

  # -----------------------------------------------------------
  # boot
  # -----------------------------------------------------------
  boot.loader = {
    efi.canTouchEfiVariables = true;
    grub = {
      enable = true;
      efiSupport = true;
      device = "nodev";
      useOSProber = true;
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
    hostName = "${def.username}-${host}";
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
  };

  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
  };

  # -----------------------------------------------------------
  # user
  # -----------------------------------------------------------
  users.users.${def.username} = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
      "uinput"
      "libvirtd"
      "audio"
      "video"
    ];
  };

  # -----------------------------------------------------------
  # packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools/deps
    gcc
    vulkan-tools
    vulkan-loader
    vulkan-validation-layers
    zenity
    libnotify
    libsecret
    wine
    wineWowPackages.stagingFull
    samba
    winetricks
    # language QOL
    alejandra
    nixd
    asm-lsp
    # cli
    ripgrep
    pciutils
    btop
    ncdu
    # web/net
    wget
    git
    curl
    # media/files
    file-roller
    p7zip
    pavucontrol
    v4l-utils
    ffmpeg
  ];

  # programs
  programs = {
    dconf.enable = true;
    xfconf.enable = true;
    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
      ];
    };
  };

  # -----------------------------------------------------------
  # theming
  # -----------------------------------------------------------
  stylix = {
    enable = true;
    autoEnable = true;
    homeManagerIntegration.autoImport = true;
    image = def.wallpaper;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/classic-dark.yaml";

    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;

    fonts.monospace.package = pkgs.nerd-fonts.jetbrains-mono;
    fonts.monospace.name = "JetBrainsMono Nerd Font Mono";

    fonts.sansSerif.package = pkgs.noto-fonts-cjk-sans;
    fonts.sansSerif.name = "Noto Sans CJK";

    fonts.serif.package = pkgs.noto-fonts-cjk-serif;
    fonts.serif.name = "Noto Serif CJK";

    fonts.sizes = {
      applications = 12;
      terminal = 12;
      desktop = 11;
      popups = 12;
    };
    targets.grub.enable = false;
  };

  # -----------------------------------------------------------
  # security & polkit
  # -----------------------------------------------------------
  security = {
    polkit.enable = true;
    rtkit.enable = true; # for sound
  };
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "prohibit-password";
    };
  };

  # -----------------------------------------------------------
  # global drivers
  # -----------------------------------------------------------

  # audio
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # bluetooth
  hardware = {
    bluetooth.enable = true;
    bluetooth.powerOnBoot = true;
  };
  services.blueman.enable = true;

  # opengl - renamed to graphics as of 24.11
  hardware = {
    graphics.enable = true;
    graphics.enable32Bit = true;
  };

  # tablet support
  programs.weylus.enable = true;
  services.udev.extraRules = ''
    KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
  '';

  # misc
  services = {
    displayManager.ly.enable = true;
    printing.enable = true;
    fstrim.enable = true;
    tumbler.enable = true;
    gvfs.enable = true;
  };
}
