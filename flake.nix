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
    ...
  } @ inputs: {
    # goat - desktop profile
    nixosConfigurations.goat = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./hardware/goat-hardware-configuration.nix
        ./modules/default-config.nix
        inputs.home-manager.nixosModules.home-manager
        inputs.stylix.nixosModules.stylix
        {
          # modules
          nvidia.enable = true;
          # user
          networking.hostName = "goat";
        }
      ];
    };
    # caprine - macbook profile
    nixosConfigurations.caprine = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {inherit inputs;};
      modules = [
        ./hardware/caprine-hardware-configuration.nix
        ./modules/default-config.nix
        inputs.home-manager.nixosModules.home-manager
        inputs.stylix.nixosModules.stylix
        {
          # modules
          nvidia.enable = false;
          # user
          networking.hostName = "caprine";
        }
      ];
    };
  };
}
