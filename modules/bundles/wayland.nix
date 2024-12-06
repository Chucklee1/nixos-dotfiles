{
  pkgs,
  inputs,
  lib,
  config,
  ...
}: {
  options = {
    wayland.enable = lib.mkEnableOption "enable wayland base";
  };

  config = lib.mkIf config.wayland.enable {
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
    ];

    # -----------------------------------------------------------
    # home manager
    # -----------------------------------------------------------
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          # wm stuff
          libnotify
          libsecret
          seahorse
          papirus-icon-theme
          (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
          swww
        ];

        programs = {
          fuzzel.enable = true;
          wlogout.enable = true;
        };
      }
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
}
