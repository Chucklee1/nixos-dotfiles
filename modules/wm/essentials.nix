{pkgs, ...}: let
  NVIDIA_WAYLAND = (niri.enable || hyprland.enable) && nvidia.enable;
in {
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
        # xdg
        XDG_SESSION_TYPE = "wayland";
        # toolkit backend
        CLUTTER_BACKEND = "wayland";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
        # xwayland compat.
        DISPLAY = ":0";
        SDL_VIDEODRIVER = "x11";
        QT_QPA_PLATFORM = "wayland;xcb";
        GDK_BACKEND = "wayland,x11,*";
        # nvidia
        GBM_BACKEND = NVIDIA_WAYLAND "nvidia_drm";
        __GLX_VENDOR_LIBRARY_NAME = NVIDIA_WAYLAND "nvidia";
        LIBVA_DRIVER_NAME = NVIDIA_WAYLAND "nvidia";
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

  # nvidia compat with wayland
  hardware.nvidia.modesetting.enable = NVIDIA_WAYLAND true;
  security.polkit.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };
}
