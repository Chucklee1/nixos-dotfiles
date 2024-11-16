{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [
    ./niri.nix
  ];
  home.sessionVariables = {
    XDG_CURRENT_DESKTOP = "niri";
    XDG_SESSION_DESKTOP = "niri";
    XDG_SESSION_TYPE = "wayland";
    GDK_BACKEND = "wayland";
    GTK_CSD = "0";
    CLUTTER_BACKEND = "wayland";
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    SDL_VIDEODRIVER = "wayland";
    MOZ_ENABLE_WAYLAND = "1";
    NIXOS_OZONE_WL = "1";
  };
  # user theming
  gtk.iconTheme.name = "Papirus-Dark";
  gtk.iconTheme.package = pkgs.papirus-icon-theme;

  # packages
  home.packages = with pkgs; [
    firefox
    vscode-fhs
    musescore
    wineWowPackages.waylandFull
    # wm stuff
    libnotify
    dunst
    swaylock-effects
    swayidle
    swww
  ];

  # smaller dotfiles
  programs = {
    lazygit.enable = true;
    fuzzel.enable = true;
    wlogout.enable = true;
    git = {
      enable = true;
      userEmail = "cooperkang4@gamil.com";
      userName = "Chucklee1";
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
        active_tab_font_style   bold
        inactive_tab_font_style bold
      '';
    };
    oh-my-posh = {
      enable = true;
      enableBoshIntigration = true;
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
    bash = {
      enable = true;
      shellAliases = {
        sv = "sudo nvim";
        v = "nvim";
        exec-swww = "swww init && swww img ~/nixos-dotfiles/wallpapers/mono-forest.PNG";
        ozonify = "--enable-features=UseOzonePlatform --ozone-platform=wayland";
        cg = "sudo nix-collect-garbage";
        update-desktop = "sudo nixos-rebuild switch --flake .#desktop --show-trace";
        update-macbook = "sudo nixos-rebuild switch --flake .#macbook --show-trace";
      };
    };
  };

  # D O  N O T  C H A N G E
  home.stateVersion = "24.05";
}
