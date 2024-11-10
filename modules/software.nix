{
  pkgs,
  lib,
  config,
  ...
}: {
  programs = {
    thunar.enable = true;
    thunar.plugins = with pkgs.xfce; [
      thunar-archive-plugin
      thunar-volman
    ];
    lazygit.enable = true;
  };

  environment.systemPackages = with pkgs; [
    # Development Tools
    ripgrep
    alejandra
    nixd
    libgccjit
    rustc

    # Command-Line Utilities
    killall
    pciutils
    sl
    cowsay
    neofetch

    # Web & Networking Utilities
    wget
    git
    curl

    # Compression & Archiving
    unrar
    unzip
    file-roller
    tree
    isoimagewriter
  ];
}
