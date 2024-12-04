{
  lib,
  config,
  ...
}: {
  imports = [
    ./system/bootloader.nix
    ./system/nix.nix
    ./system/specifics.nix
    ./system/user.nix

    ./theming/stylix.nix
    ./theming/bootloader.nix

    ./infastructure.nix
    ./packages.nix
    ./gamse.nix

    ./programs/thunar.nix
    ./programs/niri.nix

    # toggle modules #
    ./drivers/nvidia.nix
    ./programs/wayland.nix
    ./programs/vscode.nix
  ];
}
