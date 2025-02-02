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
          };
        };
        modules = let
          niri = (import ./modules/niri.nix).niri;
        in [
          ./modules/hardware.mod.nix
          ./modules/software.mod.nix
          ./modules/system.mod.nix
          ./modules/theming.mod.nix
          ./modules/hosts/${host}.nix
          niri.base
          inputs.stylix.nixosModules.stylix
          inputs.home-manager.nixosModules.home-manager
          {home-manager.sharedModules = [niri.home];}
        ];
      };
  in {
    # mkSystem declarations
    nixosConfigurations.desktop = mkSystem "desktop";
    nixosConfigurations.laptop = mkSystem "laptop";
  };
}
