{
  pkgs,
  config,
  ...
}: {
  # -----------------------------------------------------------
  # packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # dev tools
    ripgrep
    nixd
    # building utils
    cmake
    meson
    cpio
    # cli utils
    killall
    pciutils
    # web & net utils
    wget
    git
    curl
    networkmanagerapplet
    # compresssion & archiving
    unrar
    unzip
    file-roller
    tree
    isoimagewriter
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
  # programs
  # -----------------------------------------------------------
  programs = {
    thunar.enable = true;
    thunar.plugins = with pkgs.xfce; [thunar-archive-plugin thunar-volman];
    niri.enable = true;
    niri.package = pkgs.niri-unstable;
  };
}
