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
    ./dwm.nix
    ./virt.nix
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
    firewall = {
      enable = true;
      allowedTCPPorts = [1701 1901];
      allowedUDPPortRanges = [
        {
          from = 400;
          to = 4007;
        }
        {
          from = 8000;
          to = 8010;
        }
      ];
    };
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
    extraGroups = ["wheel" "networkmanager" "libvirtd" "uinput"];
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
      ./wayland.home.nix
      ./niri.home.nix
      ./waybar.home.nix
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
    vulkan-tools
    vulkan-loader
    vulkan-validation-layers
    zenity
    libnotify
    libsecret
    # wine
    wineWowPackages.stagingFull
    samba
    winetricks
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
    wev
    # xwayland
    xwayland
    xwayland-run
    # x11
    rofi
    # clipboard
    xsel
    xclip
    wl-clipboard
    # media
    mpv
    imv
    pavucontrol
    v4l-utils
    ffmpeg
    # misc
    osu-lazer-bin
    nerd-fonts.symbols-only
    file-roller
    p7zip
    dwarf-fortress
    dwarf-fortress-packages.soundSense
  ];

  # -----------------------------------------------------------
  # system programs
  # -----------------------------------------------------------
  programs = {
    dconf.enable = true;
    niri = {
      enable = true;
      package = pkgs.niri-unstable;
    };
  };

  # -----------------------------------------------------------
  # thunar
  # -----------------------------------------------------------
  programs = {
    xfconf.enable = true; # for thunar config
    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
      ];
    };
  };
  # services for thunar
  services = {
    tumbler.enable = true;
    gvfs.enable = true;
  };

  # -----------------------------------------------------------
  # security & polkit
  # -----------------------------------------------------------
  xdg.portal.enable = true;
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
    printing.enable = true;
    fstrim.enable = true;
  };
}
