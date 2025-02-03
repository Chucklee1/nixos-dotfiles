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
  } @ inputs: let
    # general declarations
    system = "x86_64-linux";

    # module helper function
    modules = let
      recursiveImport = builtins.listToAttrs (
        map (file: {
          name = builtins.replaceStrings [".nix"] [""] file;
          value = import "${self}/modules/${file}" {inherit inputs;};
        })
        (builtins.attrNames (builtins.readDir "${self}/modules"))
      );

      # lazy home wrapping
      homeWrapper = modList: [{home-manager.sharedModules = builtins.concatLists modList;}];
    in
      with recursiveImport; {
        global = builtins.concatLists [
          sysConf.global
          software.nixPkgs
          software.wine
          software.steam
          hardware.gpuGlobal
          niri.base
          theming.base
          (homeWrapper [
            niri.home
            nixvim.merged
          ])
          [
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
  in let
    # mkSystem blob
    mkSystem = host:
      nixpkgs.lib.nixosSystem {
        system = {inherit system;};
        specialArgs = {
          inherit system inputs;
          def = {
            inherit host;
            username = "goat";
            wallpaper = "${self}/assets/wallpaper.png";
          };
        };
        modules = modules.${host} ++ ["${self}/modules/${host}.gen.nix"];
      };
  in {
    # formatter
    formatter.${system} = nixpkgs.legacyPackages.${system}.alejandra;

    # mkSystem declarations
    nixosConfigurations.desktop = mkSystem "desktop";
    nixosConfigurations.laptop = mkSystem "laptop";
  };
}
