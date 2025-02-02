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
    importHomeMods = mods: [{home-manager.sharedModules = builtins.concatLists mods;}];

    modules = {
      global = builtins.concatLists [
        (importMod "system").global
        (importMod "hardware").gpuGlobal
        (importMod "niri").base
        (importHomeMods [
          (importMod "niri").home
          (importMod "nixvim").merged
        ])
        [
          ./modules/software.mod.nix
          ./modules/theming.mod.nix
          inputs.stylix.nixosModules.stylix
          inputs.home-manager.nixosModules.home-manager
        ]
      ];
      laptop = modules.global ++ (importMod "hardware").radeon;

      desktop =
        modules.global
        ++ (importMod "system").desktop
        ++ (importMod "system").virt
        ++ (importMod "hardware").nvidia
        ++ (importMod "hardware").wayVidia
        ++ (importMod "hardware").weylus
        ++ (importMod "hardware").ntfs;
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
        modules = modules.${host} ++ [./modules/hosts/${host}.nix];
      };

    # mkSystem declarations
    nixosConfigurations.desktop = mkSystem "desktop";
    nixosConfigurations.laptop = mkSystem "laptop";
  };
}
