{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    stylix,
    niri,
    ...
  } @ inputs: {
    # caprine - macbook profile
    nixosConfigurations.caprine = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hardware/caprine-hardware-configuration.nix
        ./modules/default.nix
        stylix.nixosModules.stylix
        niri.homeModules.niri
        home-manager.nixosModules.home-manager
        {
          # modules
          nvidia.enable = false;
          # user
          networking.hostName = "caprine";
          users.users.caprine = {
            isNormalUser = true;
            extraGroups = ["wheel" "networkmanager"];
          };
          # home manager user specific configs
          home-manager = {
            users.caprine = {
              imports = [../../modules/home/default.nix];
              home.username = "caprine";
              home.homeDirectory = "/home/caprine";
            };
          };
        }
      ];
    };
    # goat - desktop profile
    nixosConfigurations.goat = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hardware/goat-hardware-configuration.nix
        ./modules/default.nix
        stylix.nixosModules.stylix
        home-manager.nixosModules.home-manager
        {
          # modules
          nvidia.enable = true;
          # user
          networking.hostName = "goat";
          users.users.goat = {
            isNormalUser = true;
            extraGroups = ["wheel" "networkmanager"];
          };
          # home manager user specific configs
          home-manager = {
            users.goat = {
              imports = [../../modules/home/default.nix];
              home.username = "goat";
              home.homeDirectory = "/home/goat";
            };
          };
        }
      ];
    };
  };
}
