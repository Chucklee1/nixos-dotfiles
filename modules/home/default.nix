{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [./waybar.nix];

  # startup
  home.startupCommands = ''
      dunst
      swww init
      waybar
      lxqt-policykit-agent
    swayidle \
      timeout 300 'swaylock -f -c 000000' \   # Locks screen after 5 minutes of inactivity
      timeout 600 'swaymsg "output * dpms off"' \  # Turns off screen after 10 minutes
      resume 'swaymsg "output * dpms on"' &    # Turns screen back on upon activity
  '';
  # sym linking
  home.file.".config/niri/config.kdl".source = ../../home/niri.kdl;

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
    swaylock
    swayidle
    libnotify
    dunst
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
        exec-waybar = "pkill waybar && waybar &";
        exec-swww = "pkill swww && swww init && swww img ~/nixos-dotfiles/home-folder/pictures/wallpapers/mono-forest.PNG";
        wayland-code = "code --enable-features=UseOzonePlatform --ozone-platform=wayland";
        cg = "sudo nix-collect-garbage";
        update-caprine = "sudo nixos-rebuild switch --flake ~/nixos-dotfiles#caprine --show-trace";
        update-goat = "sudo nixos-rebuild switch --flake ~/nixos-dotfiles#goat --show-trace";
      };
    };
  };
}
