{
  pkgs,
  inputs,
  lib,
  config,
  ...
}: {
  # -----------------------------------------------------------
  # system - software
  # -----------------------------------------------------------

  nixpkgs.overlays = [inputs.niri.overlays.niri];
  programs = {
    niri.enable = true;
    niri.package = pkgs.niri-unstable;
  };

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
    networkmanagerapplet
  ];

  # -----------------------------------------------------------
  # home manager
  # -----------------------------------------------------------
  home-manager.users.goat = {
    imports = [
      ./config.kdl.nix
    ];
    stylix.targets = {
      niri.enable = true;
      waybar.enable = false;
    };
    home.packages = with pkgs; [
      # wm stuff
      libnotify
      libsecret
      seahorse
      papirus-icon-theme
      (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
      # in settings.nix startup
      swww
      dunst
      wlsunset
    ];

    programs = {
      fuzzel.enable = true;
      wlogout.enable = true;
      waybar = {
        enable = true;
        systemd.enable = true;
      };
    };
  };

  # -----------------------------------------------------------
  # system - security & policy
  # -----------------------------------------------------------
  services.gnome.gnome-keyring.enable = true;
  security = {
    rtkit.enable = true; # enable rtkit for sound
    polkit.enable = true; # enable policykit
    pam.services.swaylock = {
      text = ''
        auth include login
      '';
    };
  };

  systemd = {
    user.services.lxqt-policykit-agent = {
      description = "LXQt PolicyKit Agent";
      serviceConfig.ExecStart = "${pkgs.lxqt.lxqt-policykit}/bin/lxqt-policykit-agent";
      wantedBy = ["default.target"];
    };
  };

  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    configPackages = [
      pkgs.xdg-desktop-portal-gtk
      pkgs.xdg-desktop-portal
    ];
  };
}
