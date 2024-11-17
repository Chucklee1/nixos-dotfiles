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
  } @ inputs: let
    shared-modules = [
      ./modules/default-config.nix
      inputs.home-manager.nixosModules.home-manager
      inputs.stylix.nixosModules.stylix
      inputs.niri.nixosModules.niri
      grub2-themes.nixosModules.default
    ];
  in {
    # desktop profile
    nixosConfigurations.desktop = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {
        inherit inputs;
        niri.enable = true;
      };
      modules =
        shared-modules
        ++ [
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
      specialArgs = {
        inherit inputs;
      };
      modules =
        shared-modules
        ++ [
          ./modules/hardware/macbook.nix
          {
            nvidia.enable = false;
            steam.enable = false;
            niri.enable = false;
          }
        ];
    };
  };
}
# small notes:
# - default order parameters - { pkgs, inputs, lib, config, ... }

