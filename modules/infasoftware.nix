{pkgs, ...}: {
  # -----------------------------------------------------------
  # system packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # dev tools
    ripgrep
    alejandra
    nixd
    libgccjit
    rustc

    # cli utils
    killall
    pciutils
    sl
    cowsay
    neofetch

    # web/net utils
    wget
    git
    curl
    networkmanagerapplet

    # compresssion/archiving
    unrar
    unzip
    file-roller
    tree
    isoimagewriter
  ];

  # -----------------------------------------------------------
  # home packages
  # -----------------------------------------------------------
  home-manager.users.goat.home.packages = with pkgs; [
    firefox
    vscode-fhs
    musescore
    wineWowPackages.waylandFull
  ];

  # -----------------------------------------------------------
  # system programs
  # -----------------------------------------------------------
  programs = {
    # File Manager
    thunar.enable = true;
    thunar.plugins = with pkgs.xfce; [
      thunar-archive-plugin
      thunar-volman
    ];
  };

  # -----------------------------------------------------------
  # home programs
  # -----------------------------------------------------------
  home-manager.users.goat.programs = {
    kitty = {
      enable = true;
      settings = {
        scrollback_lines = 2000;
        wheel_scroll_min_lines = 1;
        window_padding_width = 4;
        confirm_os_window_close = 0;
      };
      extraConfig = ''
        tab_bar_style fade
        tab_fade 1
        active_tab_font_style bold
        inactive_tab_font_style bold
      '';
    };
    oh-my-posh = {
      enable = true;
      enableBashIntegration = true;
      useTheme = "pure";
    };
    neovim = {
      enable = true;
      defaultEditor = true;
      viAlias = true;
      vimAlias = true;
      extraConfig = ''
        set clipboard=unnamedplus
        set number
        set tabstop=2
        set shiftwidth=2
        set expandtab  " Use spaces instead of tabs
      '';
    };
    git = {
      enable = true;
      userEmail = "cooperkang4@gamil.com";
      userName = "Chucklee1";
    };
  };

  # -----------------------------------------------------------
  # hardware
  # -----------------------------------------------------------
  hardware = {
    graphics.enable = true;
    bluetooth.enable = true;
    bluetooth.powerOnBoot = true;
  };

  # -----------------------------------------------------------
  # sound
  # -----------------------------------------------------------
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # -----------------------------------------------------------
  # display manager
  # -----------------------------------------------------------
  services.displayManager = {
    enable = true;
    ly.enable = true;
  };

  # -----------------------------------------------------------
  # misc services
  # -----------------------------------------------------------
  services = {
    printing.enable = true;
    gvfs.enable = true;
    tumbler.enable = true;
    fstrim.enable = true;
    gnome.gnome-keyring.enable = true;
    openssh.enable = true;
  };
}
