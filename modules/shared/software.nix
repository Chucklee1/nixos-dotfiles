{pkgs, ...}: {
  # -----------------------------------------------------------
  # packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools/deps
    gcc
    vulkan-tools
    ffmpeg
    v4l-utils
    libnotify
    libsecret
    # language QOL
    alejandra
    nixd
    asm-lsp
    # cli
    killall
    ripgrep
    pciutils
    btop
    ncdu
    # web/net
    wget
    git
    curl
    # wayland
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland
    # window manager utils
    wev
    brightnessctl
    xclip
    wl-clipboard
    cliphist
    swaybg
    wlsunset
    networkmanagerapplet
    # xwayland
    xwayland
    xwayland-run
    xsel
    xclip
    # media
    mpv
    imv
    pavucontrol
    # apps/games
    firefox
    #openmw
    # misc
    nerd-fonts.symbols-only
    file-roller
    p7zip
  ];

  # -----------------------------------------------------------
  # programs
  # -----------------------------------------------------------
  programs = {
    niri.enable = true;
    xfconf.enable = true; # for thunar config
    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
      ];
    };
  };
}
