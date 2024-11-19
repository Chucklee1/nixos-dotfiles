{inputs, ...}: {
  imports = [
    # flake inputs
    inputs.home-manager.nixosModules.home-manager
    inputs.stylix.nixosModules.stylix
    inputs.niri.nixosModules.niri
    grub2-themes.nixosModules.default
    # toggle modules
    ./GPU/nvidia.nix
    ./wayland/niri.nix
    ./gamse.nix
    # the rest
    ./system.nix
    ./infasoftware.nix
    ./theming.nix
  ];
}
