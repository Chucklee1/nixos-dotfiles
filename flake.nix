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

  outputs = {nixpkgs, ...} @ inputs: rec {
    # needs to be outside of mkSystem function
    system = "x86_64-linux";

    # import fun
    importMod = mod: (import ./modules/${mod}.nix {inherit inputs;}).${mod};
    niri = importMod "niri";
    nixvim = importMod "nixvim";

    # mkSystem blob
    mkSystem = host:
      nixpkgs.lib.nixosSystem {
        system = {inherit system;};
        specialArgs = {
          inherit system inputs;
          def = {
            inherit host;
            username = "goat";
            wallpaper = ./assets/wallpaper.png;
          };
        };
        modules = let
        in
          niri.base
          ++ nixvim.merged
          ++ [
            ./modules/hardware.mod.nix
            ./modules/software.mod.nix
            ./modules/system.mod.nix
            ./modules/theming.mod.nix
            ./modules/hosts/${host}.nix
            inputs.stylix.nixosModules.stylix
            inputs.home-manager.nixosModules.home-manager
            {home-manager.sharedModules = niri.home;}
          ];
      };

    # mkSystem declarations
    nixosConfigurations.desktop = mkSystem "desktop";
    nixosConfigurations.laptop = mkSystem "laptop";
  };
}
