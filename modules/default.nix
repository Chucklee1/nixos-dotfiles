{...}: {
  imports = [
    ./general/games.nix #
    ./general/hardware.nix #
    ./general/system.nix # main config
    ./general/stylix.nix

    ./programs/bash.nix
    ./programs/git.nix
    ./programs/terminal.nix
    ./programs/thunar.nix
    ./programs/neovim.nix
    ./programs/vscode.nix

    ./wm/essentials.nix
    ./wm/niri.nix #
    ./wm/waybar.nix #
  ];
}
