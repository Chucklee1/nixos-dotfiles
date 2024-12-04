{
  lib,
  config,
  ...
}: {
  imports = [
    ./infastructure.nix
    ./packages.nix
    ./theming.nix
    ./gamse.nix

    ./system/bootloader.nix
    ./system/nix.nix
    ./system/specifics.nix
    ./system/user.nix

    ./programs/thunar.nix
    ./programs/bash.nix
    ./programs/git.nix
    ./programs/kitty.nix
    ./programs/neovim.nix

    # toggle modules #
    ./drivers/nvidia.nix
    ./wayland.nix
    ./programs/niri.nix
    ./programs/vscode.nix
  ];
}
