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
    mod = {
      nix = mod: (import ./modules/${mod}.nix {inherit inputs;});
      home = mod: {home-manager.sharedModules = mod.nix "${mod}";};
    };

    modules = {
      global = [
        (mod.nix "hardware").gpuGlobal
        (mod.nix "niri").base
        (mod.home "niri").home
        (mod.home "nixvim").merged
        ./modules/software.mod.nix
        ./modules/system.mod.nix
        ./modules/theming.mod.nix
        inputs.stylix.nixosModules.stylix
        inputs.home-manager.nixosModules.home-manager
      ];
      laptop = [
        (mod.nix "hardware").radeon
      ];
      desktop = with (mod.nix "hardware"); [
        nvidia
        weylus
        ntfs
      ];
    };

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
        modules =
          modules.global
          ++ modules.${host}
          ++ [./modules/hosts/${host}.nix];
      };

    # mkSystem declarations
    nixosConfigurations.desktop = mkSystem "desktop";
    nixosConfigurations.laptop = mkSystem "laptop";
  };
}
