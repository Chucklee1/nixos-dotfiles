{pkgs, ...}: {
  # -----------------------------------------------------------
  # system
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # dev stuff
    ripgrep
    alejandra
    nixd
    # buildiers/compilers
    cmake
    meson
    cpio
    # cli
    killall
    pciutils
    btop
    ncdu
    # web/net
    wget
    git
    curl
    # compresssion, archiving, & filed
    unrar
    unzip
    xarchiver
    tree
    isoimagewriter
    # wayland
    wayland-utils
    wayland-scanner
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland
    # clipboard
    wl-clipboard
    cliphist
    # media
    mpv
    imv
    ffmpeg
    v4l-utils
    # hardware I/O tools
    wev
    pavucontrol
    # libs
    libnotify
    libsecret
    # cli fun
    neofetch
    sl
    cowsay
    # apps
    firefox
  ];
}
