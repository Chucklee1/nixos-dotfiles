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

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: rec {
    modules = let
      recursiveImport = builtins.listToAttrs (
        map (mod: {
          name = mod;
          value = import "${self}/modules/${mod}.nix" {inherit inputs;};
        }) (map
          (file: builtins.replaceStrings [".nix"] [""] file)
          (builtins.attrNames (builtins.readDir "${self}/modules")))
      );

      homeModWrapper = mods: [{home-manager.sharedModules = builtins.concatLists mods;}];
    in
      with recursiveImport; {
        global = builtins.concatLists [
          sysConf.global
          software.nixPkgs
          software.wine
          software.steam
          hardware.gpuGlobal
          niri.base
          (homeModWrapper [
            niri.home
            nixvim.merged
          ])
          [
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
          sysConf.desktop
          sysConf.virt
          hardware.nvidia
          hardware.wayVidia
          hardware.weylus
          hardware.ntfs
        ];
      };

    # needs to be outside of mkSystem function
    system = "x86_64-linux";

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
        modules = modules.${host} ++ [./modules/${host}.gen.nix];
      };

    # mkSystem declarations
    nixosConfigurations.desktop = mkSystem "desktop";
    nixosConfigurations.laptop = mkSystem "laptop";
  };
}
