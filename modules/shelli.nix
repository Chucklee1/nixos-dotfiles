{pkgs, ...}: {
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
        window_padding_width = 4;
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
        v = "nvim ./nixos-dotfiles";
        vi = "nvim ./nixos-dotfiles";
        vim = "nvim ./nixos-dotfiles";
        sv = "sudo nvim";
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
