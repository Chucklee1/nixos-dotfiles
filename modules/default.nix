{
  lib,
  config,
  ...
}: {
  imports = [
    ./system/bootloader.nix
    ./system/system-settings.nix
    ./system/user.nix

    ./bundles/infastructure.nix
    ./bundles/packages.nix
    ./bundles/theming.nix
    ./bundles/games.nix

    ./programs/thunar.nix
    ./programs/niri.nix #
    ./programs/waybar.nix #
    ./programs/bash.nix
    ./programs/flatpak.nix
    ./programs/git.nix
    ./programs/kitty.nix
    ./programs/neovim.nix
    ./programs/vscode.nix #

    ./drivers/nvidia.nix #
    ./drivers/nvidia-wayland.nix #
    ./drivers/radeon.nix #
  ];
}
