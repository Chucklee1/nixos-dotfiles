{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
    grub2-themes.url = "github:vinceliuice/grub2-themes";
  };

  outputs = {
    self,
    nixpkgs,
    grub2-themes,
    ...
  } @ inputs: {
    # desktop profile
    nixosConfigurations.desktop = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./modules/default-config.nix
        ./modules/hardware/desktop.nix
        {
          nvidia.enable = true;
          steam.enable = true;
          niri.enable = true;
        }
      ];
    };
    # macbook profile
    nixosConfigurations.macbook = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./modules/default-config.nix
        ./modules/hardware/macbook.nix
        {
          nvidia.enable = false;
          steam.enable = false;
          niri.enable = true;
        }
      ];
    };
  };
}
# small notes:
# - default order parameters - { pkgs, inputs, lib, config, ... }

