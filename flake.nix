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

    # modulalisation
    importMod = mod: (import ./modules/${mod}.nix {inherit inputs;});
    importHomeMod = mod: [{home-manager.sharedModules = importMod mod;}];

    modules = host: {
      global = builtins.concatLists [
        (importMod "system").global
        (importMod "hardware").gpuGlobal
        (importMod "niri").base
        (importHomeMod "niri").home
        (importHomeMod "nixvim").merged
        [
          ./modules/software.mod.nix
          ./modules/theming.mod.nix
          inputs.stylix.nixosModules.stylix
          inputs.home-manager.nixosModules.home-manager
        ]
      ];
      laptop = (importMod "hardware").radeon;

      desktop = with (importMod "hardware");
        (importMod "system".desktop)
        ++ (importMod "system".virt)
        ++ nvidia
        ++ wayVidia
        ++ weylus
        ++ ntfs;
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
        modules = modules.global ++ modules.${host} ++ [./modules/hosts/${host}.nix];
      };

    # mkSystem declarations
    nixosConfigurations.desktop = mkSystem "desktop";
    nixosConfigurations.laptop = mkSystem "laptop";
  };
}
