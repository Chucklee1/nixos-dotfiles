{pkgs, ...}: let
  nasm-64 = pkgs.writeShellApplication {
    name = "nasm-64";
    runtimeInputs = [pkgs.nasm pkgs.gcc];
    text = ''
      #!/bin/bash
      name=$1
      nasm -f elf64 "$name.asm" -o "$name.o"
      ld -o "$name" "$name.o"
      rm "$name.o"
    '';
  };
in {
  # -----------------------------------------------------------
  # system
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # dev tools
    gcc
    nasm
    # language tools
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
    # scripts
    nasm-64
  ];
}
