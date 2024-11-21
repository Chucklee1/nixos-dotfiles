{
  pkgs,
  inputs,
  lib,
  config,
  ...
}: {
  # -----------------------------------------------------------
  # niri
  # -----------------------------------------------------------
  nixpkgs.overlays = [inputs.niri.overlays.niri];
  programs.niri.enable = true;
  programs.niri.package = pkgs.niri-unstable;

  home-manager.users.goat = {
    # nested niri.settings so config.lib.niri.actions will work
    imports = [./settings.nix];
    # stylix targets
    stylix.targets.niri.enable = true;
    stylix.targets.waybar.enable = false;
  };

  # -----------------------------------------------------------
  # system - packaes
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
  ];

  # -----------------------------------------------------------
  # home - packages
  # -----------------------------------------------------------
  home-manager.users.goat.home.packages = with pkgs; [
    libnotify
    libsecret
    seahorse
    (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
    # in settings.nix startup
    swww
    dunst
    wlsunset
  ];

  # -----------------------------------------------------------
  # home - programs
  # -----------------------------------------------------------
  home-manager.users.goat.programs = {
    wlogout.enable = true;
    fuzzel.enable = true;
    swaylock.enable = true;
    swaylock.package = pkgs.swaylock-effects;
    waybar = {
      enable = true;
      systemd.enable = true;
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

  systemd.user.services.lxqt-policykit-agent = {
    description = "LXQt PolicyKit Agent";
    serviceConfig.ExecStart = "${pkgs.lxqt.lxqt-policykit}/bin/lxqt-policykit-agent";
    wantedBy = ["default.target"];
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