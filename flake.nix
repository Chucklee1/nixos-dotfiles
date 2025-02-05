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
      recursiveImport = concatMapAttrs (file: type: {
        ${replaceStrings [".nix"] [""] file} = import "${dir}/${file}" {inherit inputs;};
      }) (builtins.readDir dir);
      homeWrapper = modList: [{home-manager.sharedModules = concatLists modList;}];

      merge =
        (foldl' (
          acc: name:
            acc
            // {
              ${name} = foldl' (a: b: a ++ b) [] (map (mod: mod.${name} or []) (attrValues attrSet));
            }
        ) {} ["nix" "home"])
        recursiveImport;

      mergeMods = with merge;
        profile: concatList [nix.global nix.${profile} (homeWrapper [home.global home.${profile}])];
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
          modules = (mergeMods host) ++ ["${dir}/${host}.gen.nix"];
        };
    in {
      # formatter
      formatter.${system} = nixpkgs.legacyPackages.${system}.alejandra;

      # mkSystem declarations
      nixosConfigurations.desktop = mkSystem "desktop";
      nixosConfigurations.laptop = mkSystem "laptop";
    };
}
