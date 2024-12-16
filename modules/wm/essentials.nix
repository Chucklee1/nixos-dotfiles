{
  config,
  lib,
  pkgs,
  ...
}: let
  nvidia_wayland_bool = lib.mkIf ((config.niri.enable || config.hyprland.enable) && config.nvidia.enable);
  nvidia_wayland_opt = lib.optionals ((config.niri.enable || config.hyprland.enable) && config.nvidia.enable);
in {
  environment.systemPackages = with pkgs; [
    # wayland
    wayland-utils
    wayland-scanner
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland
    wineWowPackages.wayland
    # window manager utils
    wev
    brightnessctl
    wl-clipboard
    cliphist
    swww
    wlsunset
    networkmanagerapplet
    lxqt.lxqt-policykit
    # reqs/libs
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
        # wayland
        XDG_SESSION_TYPE = "wayland";
        CLUTTER_BACKEND = "wayland";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
        # xwayland compat.
        DISPLAY = ":0";
        SDL_VIDEODRIVER = "x11";
        QT_QPA_PLATFORM = "wayland;xcb";
        GDK_BACKEND = "wayland,x11,*";
        # nvidia
        GBM_BACKEND = nvidia_wayland_opt "nvidia_drm";
        __GLX_VENDOR_LIBRARY_NAME = nvidia_wayland_opt "nvidia";
        LIBVA_DRIVER_NAME = nvidia_wayland_opt "nvidia";
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

  hardware.nvidia.modesetting.enable = nvidia_wayland_bool true; # nvidia compat with wayland
  security.polkit.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };
}
