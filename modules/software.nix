{pkgs, ...}: {
  # -----------------------------------------------------------
  # packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools/deps
    gcc
    zenity
    libnotify
    libsecret
    wine
    wineWowPackages.stagingFull
    samba
    winetricks
    # language QOL
    alejandra
    nixd
    asm-lsp
    # cli
    ripgrep
    pciutils
    btop
    ncdu
    # web/net
    wget
    git
    curl
    # media/files
    file-roller
    p7zip
    pavucontrol
    v4l-utils
    # apps
    krita
    webcord
    spotify
    zoom-us
  ];

  # programs
  programs = {
    dconf.enable = true;
    xfconf.enable = true;
    firefox.enable = true;
    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
      ];
    };
  };

  home-manager.sharedModules = [
    {
      programs = {
        # git
        git = {
          enable = true;
          userEmail = "cooperkang4@gmail.com";
          userName = "Chucklee1 - remote";
        };
        # terminal emulator
        kitty = {
          enable = true;
          settings = {
            scrollback_lines = 2000;
            wheel_scroll_min_lines = 1;
            window_padding_width = 0;
            confirm_os_window_close = 0;
            window_border_width = "0px";
            tab_bar_edge = "top";
            tab_bar_margin_width = "0.0";
            tab_bar_style = "fade";
            placement_strategy = "top-left";
            hide_window_decorations = true;
          };
        };
        # shell
        bash = {
          enable = true;
          shellAliases = {
            v = "nvim";
            vi = "nvim";
            vim = "nvim";
            sv = "sudo nvim";
            nixc = "cd $HOME/nixos-dotfiles & nvim";
            dwmc = "cd $HOME/dwm & nvim";
            cg = "sudo nix-collect-garbage";
            update-laptop = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#laptop";
            update-desktop = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#desktop";
          };
        };
        oh-my-posh = {
          enable = true;
          enableBashIntegration = true;
          useTheme = "pure";
        };
      };
    }
  ];
}
