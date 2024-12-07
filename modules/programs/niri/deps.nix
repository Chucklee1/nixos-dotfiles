{
  lib,
  config,
  pkgs,
  inputs,
  ...
}: {
  config = lib.mkIf config.niri.enable {
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

    home-manager.sharedModules = [
      {
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
  };
}
