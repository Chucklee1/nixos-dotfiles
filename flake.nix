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
  } @ inputs:
    with nixpkgs.lib; let
      # general declarations
      system = "x86_64-linux";
      dir = "${self}/modules";

      # module helper function
      modules = let
        recursiveImport = listToAttrs (map (file: {
          name = replaceStrings [".nix"] [""] file;
          value = import "${dir}/${file}" {inherit inputs;};
        }) (attrNames (builtins.readDir dir)));

        # lazy home wrapping
        homeWrapper = modList: [{home-manager.sharedModules = concatLists modList;}];
      in
        with recursiveImport; {
          global = concatLists [
            sysConf.global
            software.global
            hardware.global
            niri.global
            theming.global
            (homeWrapper [
              software.home
              niri.home
              nixvim.home
            ])
          ];

          laptop = concatLists [
            modules.global
            hardware.laptop
            (homeWrapper [niri.laptop])
          ];

          desktop = concatLists [
            modules.global
            sysConf.desktop
            hardware.desktop
            software.desktop
            (homeWrapper [niri.desktop])
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
            };
          };
          modules = modules.${host} ++ ["${dir}/${host}.gen.nix"];
        };
    in {
      # formatter
      formatter.${system} = nixpkgs.legacyPackages.${system}.alejandra;

      # mkSystem declarations
      nixosConfigurations.desktop = mkSystem "desktop";
      nixosConfigurations.laptop = mkSystem "laptop";
    };
}
