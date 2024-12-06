{
  pkgs,
  inputs,
  lib,
  config,
  ...
}: {
  options = {
    niri.enable = lib.mkEnableOption "enable nvidia progam";
  };

  config = lib.mkIf config.niri.enable {
    # -----------------------------------------------------------
    # niri flake package
    # -----------------------------------------------------------
    nixpkgs.overlays = [inputs.niri.overlays.niri];
    programs = {
      niri.enable = true;
      niri.package = pkgs.niri-unstable;
    };

    # -----------------------------------------------------------
    # system
    # -----------------------------------------------------------
    environment.systemPackages = with pkgs; [
      # wayland & display utilities
      wayland
      wayland-protocols
      wayland-utils
      wayland-scanner
      egl-wayland
      qt5.qtwayland
      qt6.qtwayland
      # clipboard & clipboard management
      wl-clipboard
      cliphist
      xclip
      # media tools
      mpv
      imv
      ffmpeg
      v4l-utils
      # keyboard & input tools
      wev
      ydotool
      wtype
      # system controls
      playerctl
      pavucontrol
      brightnessctl
      # wm stuff
      libnotify
      libsecret
      seahorse
      papirus-icon-theme
      (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
    ];

    # -----------------------------------------------------------
    # portals and security
    # -----------------------------------------------------------
    services.gnome.gnome-keyring.enable = true;
    security = {
      rtkit.enable = true; # enable rtkit for sound
      polkit.enable = true; # enable policykit
    };

    xdg.portal = {
      enable = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk];
      configPackages = [
        pkgs.xdg-desktop-portal-gtk
        pkgs.xdg-desktop-portal
      ];
    };
  };

  # -----------------------------------------------------------
  # home manager
  # -----------------------------------------------------------
  home-manager.sharedModules = [
    {
      imports = [./niri-config-kdl.nix];
      # niri config packages and programs
      home.packages = with pkgs; [
        lxqt.lxqt-policykit
        dunst
        xwayland-satellite
        networkmanagerapplet
        swww
        wlsunset
      ];
      programs = {
        fuzzel.enable = true;
        wlogout.enable = true;
      };
    }
  ];
}
