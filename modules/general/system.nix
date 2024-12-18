{
  pkgs,
  lib,
  inputs,
  defaults,
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
      gtk = {
        iconTheme.name = "Papirus-Dark";
        iconTheme.package = pkgs.papirus-icon-theme;
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

  # -----------------------------------------------------------
  # system packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools
    gcc
    alejandra
    nixd
    asm-lsp
    desktop-file-utils
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
    # misc
    neofetch
    sl
    cowsay
    firefox
  ];

  # -----------------------------------------------------------
  # infastrcuture
  # -----------------------------------------------------------

  # support for non-nix executables
  programs.nix-ld.enable = true;

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
  };
}
