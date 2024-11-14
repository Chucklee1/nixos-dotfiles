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
        inputs.niri.homeManagerModules.niri
        {
          # modules
          nvidia.enable = true;
          # user hostname
          networking.hostName = "goat";
          # system user config
          users.users.goat = {
            isNormalUser = true;
            extraGroups = ["wheel" "networkmanager"];
          };
          # home manager user config
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            backupFileExtension = "backup";
            users = {
              goat = {
                imports = [
                  ./home/default-home.nix
                ];
                home.username = "goat";
                home.homeDirectory = "/home/goat";
                programs.niri.enable = true;
              };
            };
          };
        }
      ];
    };
  };
}
