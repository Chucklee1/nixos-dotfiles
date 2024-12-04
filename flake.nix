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
      ./modules/default.nix
      inputs.home-manager.nixosModules.home-manager
      inputs.stylix.nixosModules.stylix
      inputs.niri.nixosModules.niri
      grub2-themes.nixosModules.default
      {
        home-manager = {
          useUserPackages = true;
          useGlobalPkgs = true;
          extraSpecialArgs = {inherit inputs;};
          users.goat = {
            home.stateVersion = "24.05"; # DO NOT CHANGE
            home.username = "goat";
            home.homeDirectory = "/home/goat";
          };
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
            vscode.enable = true;
            wayland.enable = true;
            niri.enable = true;
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
            vscode.enable = true;
            wayland.enable = true;
            niri.enable = true;
            nvidia.enable = false;
          }
        ];
    };
  };
}
# small notes:
# - default order parameters - { pkgs, inputs, lib, config, ... }

