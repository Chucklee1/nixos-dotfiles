{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {nixpkgs, ...} @ inputs: let
    mkSystem = host:
      nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        specialArgs = {
          inherit system inputs;
          def = {
            username = "goat";
            inherit host;
            wallpaper = ./assets/wallpaper.png;
            files = import ./assets/files.nix;
            mkOpt = nixpkgs.lib.mkOption {
              type = nixpkgs.lib.types.bool;
              default = false;
              description = "read";
            };
          };
        };
        modules = [
          ./modules/options.nix
          ./modules/hardware.nix
          ./modules/software.nix
          ./modules/system.nix
          ./modules/theming.nix
          ./modules/hosts/${host}.nix
          inputs.stylix.nixosModules.stylix
          inputs.home-manager.nixosModules.home-manager
          {${host}.enable = true;}
        ];
      };
  in {
    # mkSystem declarations
    nixosConfigurations.desktop = mkSystem "desktop";
    nixosConfigurations.laptop = mkSystem "laptop";
  };
}
