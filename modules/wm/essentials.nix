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
