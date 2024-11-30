{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    impermanence.url = "github:nix-community/impermanence";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
    grub2-themes.url = "github:vinceliuice/grub2-themes";
  };

  outputs = {
    self,
    nixpkgs,
    impermanence,
    grub2-themes,
    ...
  } @ inputs: let
    shared-modules = [
      ./modules/default.nix
      inputs.home-manager.nixosModules.home-manager
      inputs.stylix.nixosModules.stylix
      inputs.niri.nixosModules.niri
      grub2-themes.nixosModules.default
      {
        nixpkgs.overlays = [inputs.niri.overlays.niri];
        home-manager = {
          useUserPackages = true;
          useGlobalPkgs = true;
          extraSpecialArgs = {inherit inputs;};
          users.goat.imports = [./modules/home/home.nix];
        };
      }
    ];
  in {
    # desktop profile
    nixosConfigurations.desktop = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules =
        shared-modules
        ++ [
          ./modules/hardware/desktop.nix
          {
            nvidia.enable = true;
          }
        ];
    };
    # laptop profile
    nixosConfigurations.laptop = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules =
        shared-modules
        ++ [
          ./modules/hardware/laptop.nix
          {
            services.xserver.videoDrivers = ["amd"];
            nvidia.enable = false;
          }
        ];
    };
  };
}
# small notes:
# - default order parameters - { pkgs, inputs, lib, config, ... }

