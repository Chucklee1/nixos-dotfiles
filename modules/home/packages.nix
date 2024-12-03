{pkgs, ...}: {
  home.packages = with pkgs; [
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

  programs.lazygit.enable = true;
}
