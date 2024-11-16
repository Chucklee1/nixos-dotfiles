{pkgs, ...}: {
  # system env packages
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
    networkmanagerapplet

    # Compression & Archiving
    unrar
    unzip
    file-roller
    tree
    isoimagewriter
  ];

  # home-manager packages
  home-manager.users.goat.home.packages = with pkgs; [
    firefox
    vscode-fhs
    musescore
    wineWowPackages.waylandFull
  ];

  # system programs
  programs = {
    thunar.enable = true;
    thunar.plugins = with pkgs.xfce; [
      thunar-archive-plugin
      thunar-volman
    ];
  };

  # home manager programs
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
        active_tab_font_style   bold
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

  # opengl option, renamed to graphics as of 24.11
  hardware.graphics.enable = true;

  # bluetooth
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.blueman.enable = true;

  # sound
  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  # display-manager
  services.displayManager = {
    enable = true;
    ly.enable = true;
  };
  # misc services
  services = {
    printing.enable = true;
    gvfs.enable = true;
    tumbler.enable = true;
    fstrim.enable = true;
    gnome.gnome-keyring.enable = true;
    openssh.enable = true;
  };
}
