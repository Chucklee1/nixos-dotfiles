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
    ./theming.mod.nix
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
  # user & home manager
  # -----------------------------------------------------------
  users.users.${def.username} = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager" "uinput" "libvirtd"];
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
      ./shelli.home.nix
      ./nixvim.home.nix
      {
        home.packages = with pkgs; [
          krita
          webcord
          spotify
          zoom-us
        ];
        programs.firefox.enable = true;
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
    #vulkan-tools
    #vulkan-loader
    #vulkan-validation-layers
    #zenity
    libnotify
    libsecret
    # wine
    #wineWowPackages.stagingFull
    #samba
    #winetricks
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
    # x11/wm
    dmenu
    xclip
    nerd-fonts.symbols-only
    # media/files
    file-roller
    p7zip
    mpv
    imv
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
  # security & polkit
  # -----------------------------------------------------------
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal];
    config.common.default = ["gtk"];
  };
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

  # X11
  services.xserver = {
    enable = true;
    xkb.layout = def.layout;
    windowManager.dwm = {
      enable = true;
      package = pkgs.dwm.overrideAttrs (oldAttrs: {
        src = builtins.path {
          path = /home/goat/nixos-dotfiles/assets/dwm;
          recursive = true;
        };
      });
    };
    displayManager.sessionCommands = ''
      ${lib.getExe pkgs.feh} --bg-scale ${def.wallpaper}
      ${lib.getExe pkgs.redshift} -O 5100
    '';
  };

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
