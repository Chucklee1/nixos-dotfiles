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
    # compresssion & archiving
    unrar
    unzip
    file-roller
    tree
    isoimagewriter
  ];

  # -----------------------------------------------------------
  # programs
  # -----------------------------------------------------------
  programs = {
    thunar.enable = true;
    thunar.plugins = with pkgs.xfce; [thunar-archive-plugin thunar-volman];
  };
}
