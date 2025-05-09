{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    niri.url = "github:sodiboo/niri-flake";
    minegrub-theme.url = "github:Lxtharia/minegrub-theme";
    minecraft-plymouth.url = "github:nikp123/minecraft-plymouth-theme";
    minesddm.url = "github:Davi-S/sddm-theme-minesddm";
    minesddm.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    # CUSTOM ARGS HERE
    user = "goat";
    system = "x86_64-linux";
    nixvim' = inputs.nixvim.legacyPackages.${system};
    lib = nixpkgs.lib;
    mylib = import "${self}/utils.nix" {inherit nixpkgs;};

    # ---- system  ----
    mkSystem = host:
      lib.nixosSystem {
        inherit system;
        modules = let
          # passing args here to inherit host
          mod' = mylib.mergeModules "${self}/modules" {
            inherit inputs self user system;
            machine = host;
          };
          mod = mylib.mergeProfiles mod' "global" "${host}";
        in
          mod.nix ++ [{_module.args.homeMods = mod.home;}];
      };

    # ---- custom pkgs ----
    nixvim = nixvim'.makeNixvimWithModule {
      module.imports = mylib.mergeModules "${self}/nixvim" {};
    };
  in {
    packages.${system} = {inherit nixvim;};

    nixosConfigurations = lib.genAttrs ["desktop" "macbook"] (host: mkSystem host);
  };
}
