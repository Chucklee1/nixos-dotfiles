{...}: {
  imports = [
    # = togglable module
    ./general/os-settings.nix
    ./general/infastructure.nix
    ./general/packages.nix
    ./general/theming.nix

    ./programs/thunar.nix
    ./programs/niri.nix # temp
    ./programs/wayland.nix # temp
    ./programs/waybar.nix #
    ./programs/bash.nix
    ./programs/git.nix
    ./programs/kitty.nix
    ./programs/neovim.nix
    ./programs/vscode.nix #

    ./drivers/nvidia.nix #
    ./drivers/radeon.nix #
  ];
}
