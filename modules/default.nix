{
  lib,
  config,
  ...
}: {
  imports = [
    ./system/bootloader.nix
    ./syste/home-manager.nix
    ./system/specifics.nix
    ./system/user.nix
    ./system/infastructure.nix
    ./system/packages.nix
    ./system/theming.nix

    ./programs/games.nix
    ./programs/thunar.nix
    ./programs/bash.nix
    ./programs/git.nix
    ./programs/kitty.nix
    ./programs/neovim.nix

    # toggle modules #
    ./system/wayland.nix
    ./drivers/nvidia.nix
    ./drivers/nvidia-wayland.nix
    ./drivers/radeon.nix
    ./programs/niri.nix
    ./programs/vscode.nix
  ];
}
