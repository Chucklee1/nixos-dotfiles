{
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    # dev tools
    openai
    python3
    gnumake
    gdb
    gcc
    # apps
    tenacity
    gimp
    spotdl
    picard
    feishin
    qbittorrent
    musescore
    libreoffice
    logisim-evolution
  ];

  programs = {
    # diagnostics
    btop.enable = true;
    mangohud.enable = true;
    # browser
    chromium = {
      enable = true;
      package = pkgs.ungoogled-chromium;
    };

    # git
    git = {
      enable = true;
      userEmail = "kermitthefrog@kakao.com";
      userName = "Chucklee1";
    };
    # terminal emulator
    kitty = {
      enable = true;
      settings = {
        confirm_os_window_close = 0;
        hide_window_decorations = true;
        tab_bar_edge = "top";
        tab_bar_style = lib.mkForce "slant";
      };
    };
    # shell
    bash = {
      enable = true;
      shellAliases = {
        cg = "nix-collect-garbage";
        update-flake = "nix flake update $HOME/nixos-dotfiles";
        rebuild-desktop = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#desktop";
        rebuild-laptop = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#laptop";
        rebuild-macbook = "sudo darwin-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#macbookpro";
      };
    };
  };
}
