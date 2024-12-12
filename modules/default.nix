{...}: {
  imports = [
    ./general/os-settings.nix
    ./general/infastructure.nix
    ./general/user.nix
    ./general/packages.nix
    ./general/security.nix
    ./general/theming.nix
    ./general/hardware.nix

    ./programs/thunar.nix
    ./programs/bash.nix
    ./programs/git.nix
    ./programs/tmux.nix
    ./programs/kitty.nix
    ./programs/neovim.nix
    ./programs/vscode.nix #

    ./programs/niri/program.nix #
    ./programs/niri/waybar.nix #
    ./programs/niri/nvidia.nix #

    ./themes/fancy.nix #
    ./themes/minimal.nix #
  ];
}
