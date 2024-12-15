{...}: {
  imports = [
    ./general/games.nix #
    ./general/hardware.nix #
    ./general/infastructure.nix
    ./general/os-settings.nix
    ./general/packages.nix
    ./general/theming.nix

    ./programs/bash.nix
    ./programs/git.nix
    ./programs/kitty.nix
    ./programs/thunar.nix
    ./programs/neovim.nix
    ./programs/niri.nix #
    ./programs/tmux.nix
    ./programs/vscode.nix
  ];
}
