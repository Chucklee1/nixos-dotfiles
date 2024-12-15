{pkgs, ...}: let
in {
  # -----------------------------------------------------------
  # system
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools
    gcc
    alejandra
    nixd
    asm-lsp
    # cli
    killall
    ripgrep
    pciutils
    zenity
    btop
    ncdu
    # web/net
    wget
    git
    curl
    # compresssion, archiving, & filed
    unrar
    unzip
    p7zip
    file-roller
    tree
    isoimagewriter
    # misc
    neofetch
    sl
    cowsay
    firefox
    sddm-chili-theme
  ];
}
