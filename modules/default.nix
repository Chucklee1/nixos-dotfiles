{...}: {
  imports = [
    ./system/bootloader.nix
    ./system/nix.nix
    ./system/specifics.nix
    ./system/user.nix
    ./theming/stylix.nix
    ./theming/bootloader.nix
    ./theming/base16scheme.nix
    ./gamse.nix
    ./infastructure.nix
    ./packages.nix
    ./programs/thunar.nix
    # toggle modules #
    ./drivers/nvidia.nix
    ./programs/niri.nix
  ];
}
