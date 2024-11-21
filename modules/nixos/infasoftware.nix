{
  pkgs,
  config,
  ...
}: {
  # -----------------------------------------------------------
  # enviorment packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # dev tools
    ripgrep
    nixd
    alejandra
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
  ];

  # -----------------------------------------------------------
  # thunar
  # -----------------------------------------------------------
  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [thunar-archive-plugin thunar-volman];
  };
  services = {
    gvfs.enable = true;
    tumbler.enable = true;
  };

  # -----------------------------------------------------------
  # theming
  # -----------------------------------------------------------
  stylix = {
    enable = true;
    homeManagerIntegration.autoImport = true;
    image = ../pictures/night-ridgeline.jpg;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/tokyo-city-terminal-dark.yaml";
    opacity.terminal = 0.6;
    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;
    fonts = {
      monospace = {
        package = pkgs.nerdfonts.override {fonts = ["JetBrainsMono"];};
        name = "JetBrainsMono Nerd Font Mono";
      };
      sansSerif = {
        package = pkgs.noto-fonts-cjk-sans;
        name = "Noto Sans CJK";
      };
      serif = {
        package = pkgs.noto-fonts-cjk-serif;
        name = "Noto Serif CJK";
      };
      sizes = {
        applications = 12;
        terminal = 12;
        desktop = 11;
        popups = 12;
      };
    };
  };

  # -----------------------------------------------------------
  # hardware
  # -----------------------------------------------------------
  hardware = {
    graphics.enable = true; # renamed opengl to graphics as of 24.11
    graphics.enable32Bit = true;
    bluetooth.enable = true;
    bluetooth.powerOnBoot = true;
  };

  # -----------------------------------------------------------
  # services
  # -----------------------------------------------------------
  services = {
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    displayManager = {
      enable = true;
      ly.enable = true;
    };
    blueman.enable = true;
    printing.enable = true;
    fstrim.enable = true;
    openssh.enable = true;
  };
}