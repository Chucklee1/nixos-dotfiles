{
  lib,
  pkgs,
  ...
}: {
  # darwin
  services.tailscale.enable = true;
  programs.bash = {
    enable = true;
    interactiveShellInit =
      #bash
      ''
        alias cg='nix-collect-garbage'
        alias update-flake='nix-flake-update'
        alias rebuild-macbook='darwin-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#macbookpro'

        eval "$(${pkgs.oh-my-posh}/bin/oh-my-posh init bash --config ${pkgs.oh-my-posh}/themes/pure.omp.json)"
      '';
  };

  # homebrew
  homebrew = {
    enable = true;
    brews = ["navidrome"];
    casks = [
      "kitty"
      "raycast"
      "musescore"
      "musicbrainz-picard"
      "submariner"
      "logisim-evolution"
    ];

    /*
      masApps = {
      "Drafts" = 1435957248;
      "Reeder" = 1529448980;
      "Things" = 904280696;
      "Timery" = 1425368544;
    };
    */
  };

  # nix
  environment.systemPackages = with pkgs; [
    # dev
    python3
    gnumake
    gdb
    gcc
  ];

  # home manager
  home-manager.users.goat.programs = {
    btop.enable = true;
    git = {
      enable = true;
      userEmail = "kermitthefrog@kakao.com";
      userName = "Chucklee1";
    };
    kitty = {
      enable = true;
      settings = {
        confirm_os_window_close = 0;
        hide_window_decorations = true;
        tab_bar_edge = "top";
        tab_bar_style = lib.mkForce "slant";
      };
    };
  };
}
