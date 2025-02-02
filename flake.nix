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

    modules = let
      # modulalisation
      importMods = builtins.listToAttrs (map (mod: {
        name = mod;
        value = import ./modules/${mod}.nix {inherit inputs;};
      }) ["_system" "hardware" "niri" "nixvim"]);

      homeModWrapper = mods: [{home-manager.sharedModules = builtins.concatLists mods;}];
    in
      with importMods; {
        global = builtins.concatLists [
          _system.global
          hardware.gpuGlobal
          niri.base
          (homeModWrapper [
            niri.home
            nixvim.merged
          ])
          [
            ./modules/software.nix
            ./modules/theming.nix
            inputs.stylix.nixosModules.stylix
            inputs.home-manager.nixosModules.home-manager
          ]
        ];
        laptop = builtins.concatLists [
          modules.global
          hardware.radeon
        ];

        desktop = builtins.concatLists [
          modules.global
          _system.desktop
          _system.virt
          hardware.nvidia
          hardware.wayVidia
          hardware.weylus
          hardware.ntfs
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
        modules = modules.${host} ++ [./modules/hosts/${host}.nix];
      };

    # mkSystem declarations
    nixosConfigurations.desktop = mkSystem "desktop";
    nixosConfigurations.laptop = mkSystem "laptop";
  };
}
