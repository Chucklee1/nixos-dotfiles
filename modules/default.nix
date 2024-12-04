{
  lib,
  config,
  ...
}: {
  imports = [
    ./system/bootloader.nix
    ./home-manager.nix
    ./system/specifics.nix
    ./system/user.nix
    ./system/infastructure.nix
    ./system/packages.nix
    ./system/theming.nix

    ./programs.games.nix
    ./programs/thunar.nix
    ./programs/bash.nix
    ./programs/git.nix
    ./programs/kitty.nix
    ./programs/neovim.nix

    # toggle modules #
    ./drivers/nvidia.nix
    ./drivers/nvidia-wayland.nix
    ./drivers/radeon.nix
    ./wayland.nix
    ./programs/niri.nix
    ./programs/vscode.nix
  ];
}
