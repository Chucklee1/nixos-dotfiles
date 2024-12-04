{
  lib,
  config,
  ...
}: {
  imports = [
    ./system/bootloader.nix
    ./system/home-manager.nix
    ./system/specifics.nix
    ./system/user.nix

    ./bundles/infastructure.nix
    ./bundles/packages.nix
    ./bundles/theming.nix
    ./bundles/games.nix
    ./bundles/wayland.nix #

    ./programs/thunar.nix
    ./programs/bash.nix
    ./programs/git.nix
    ./programs/kitty.nix
    ./programs/neovim.nix
    ./programs/niri.nix #
    ./programs/vscode.nix #

    ./drivers/nvidia.nix #
    ./drivers/nvidia-wayland.nix #
    ./drivers/radeon.nix #
  ];
}
