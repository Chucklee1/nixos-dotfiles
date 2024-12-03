{}: {
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
}
