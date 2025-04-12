{
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [inputs.nixvim.homeManagerModules.nixvim];
  stateVersion = "24.05"; # DO NOT CHANGE
  username = "goat";
  programs = {
    btop.enable = true;

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
        rebuild-darwin = "darwin-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#darwin";
      };
    };
  };
  homeDirectory = "/Users/goat";
}
