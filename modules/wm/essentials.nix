{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    # wayland
    wayland-utils
    wayland-scanner
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland
    swww
    wlsunset
    networkmanagerapplet
    lxqt.lxqt-policykit
    wev
    wl-clipboard
    cliphist
    wineWowPackages.wayland
    # utils/libs
    brightnessctl
    ffmpeg
    v4l-utils
    libnotify
    libsecret
    # media
    mpv
    imv
    pavucontrol
    # font
    (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
  ];

  home-manager.sharedModules = [
    {
      home.sessionVariables = {
        NIXOS_OZONE_WL = "1";
        XDG_SESSION_TYPE = "wayland";

        GDK_BACKEND = "wayland";
        CLUTTER_BACKEND = "wayland";
        QT_QPA_PLATFORM = "wayland;xcb";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
        QT_AUTO_SCREEN_SCALE_FACTOR = "1";

        SDL_VIDEODRIVER = "x11";
        DISPLAY = ":0";
        MOZ_ENABLE_WAYLAND = "1";
      };
      programs = {
        fuzzel.enable = true;
        wlogout.enable = true;
      };
      services = {
        gnome-keyring.enable = true;
        dunst.enable = true;
      };
    }
  ];

  security.polkit.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };
}
