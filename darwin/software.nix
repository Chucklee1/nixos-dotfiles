{pkgs, ...}: {
  # darwin
  services.tailscale.enable = true;

  environment = {
    etc."bashrc".text =
      #bash
      ''
        alias cg='nix-collect-garbage'
        alias update-flake='nix-flake-update'
        alias rebuild-macbook='darwin-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#macbook'

        eval "$(${pkgs.oh-my-posh}/bin/oh-my-posh init bash --config ${pkgs.oh-my-posh}/themes/pure.omp.json)"
      '';
    shells = [pkgs.bash pkgs.bashInteractive];
    systemPackages = with pkgs; [
      # dev
      python3
      gnumake
      gdb
      gcc
    ];
  };

  # homebrew
  homebrew = {
    enable = true;
    casks = [
      "kitty"
      "raycast"
      "musescore"
      "musicbrainz-picard"
      "submariner"
      "logisim-evolution"
    ];
  };

  /*
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
  */
}
