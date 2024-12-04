{pkgs, ...}: {
  # -----------------------------------------------------------
  # system
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
    # compresssion & archiving
    unrar
    unzip
    file-roller
    tree
    isoimagewriter
  ];

  # -----------------------------------------------------------
  # home manager
  # -----------------------------------------------------------
  home-manager.users.goat.home.packages = with pkgs; [
    # apps
    firefox
    musescore
    zoom-us
    # cli
    btop
    ncdu
    neofetch
    sl
    cowsay
    alejandra
  ];
}