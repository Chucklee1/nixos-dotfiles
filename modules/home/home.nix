{pkgs, ...}: {
  imports = [./config.kdl.nix];
  home = {
    stateVersion = "24.05"; # DO NOT CHANGE
    username = "goat";
    homeDirectory = "/home/goat";
  };
  # -----------------------------------------------------------
  # packages
  # -----------------------------------------------------------
  home.packages = with pkgs; [
    # apps
    firefox
    musescore
    zoom-us
    vscode-fhs
    # cli
    btop
    ncdu
    neofetch
    sl
    cowsay
    # wm stuff
    libnotify
    libsecret
    seahorse
    papirus-icon-theme
    (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
    # in settings.nix startup
    swww
    dunst
    wlsunset
  ];

  # -----------------------------------------------------------
  # programs
  # -----------------------------------------------------------
  programs = {
    lazygit.enable = true;
    wlogout.enable = true;
    fuzzel.enable = true;
    swaylock.enable = true;
    swaylock.package = pkgs.swaylock-effects;
    waybar = {
      enable = true;
      systemd.enable = true;
    };
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
    oh-my-posh = {
      enable = true;
      enableBashIntegration = true;
      useTheme = "pure";
    };
    bash = {
      enable = true;
      shellAliases = {
        sv = "sudo nvim";
        v = "nvim";
        kittty = "kitty working-directory $HOME/nixos-dotfiles";
        ozonify = "--enable-features=UseOzonePlatform --ozone-platform=wayland";
        cg = "sudo nix-collect-garbage";
        update-desktop = "sudo nixos-rebuild switch --flake .#desktop --show-trace";
        update-laptop = "sudo nixos-rebuild switch --flake .#laptop --show-trace";
      };
    };
  };

  # -----------------------------------------------------------
  # theming
  # -----------------------------------------------------------
  gtk = {
    enable = true;
    iconTheme.name = "Papirus nvm iDark";
  };
  stylix.targets = {
    neovim.enable = true;
    niri.enable = true;
    waybar.enable = false;
  };
}
