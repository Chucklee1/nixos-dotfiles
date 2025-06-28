{
  description = "Never let them know your next move";

  # ---- main pkg providers ----
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.home-manager.url = "github:nix-community/home-manager";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";

  # ---- disk formatting ----
  inputs.disko.url = "github:nix-community/disko";
  inputs.disko.inputs.nixpkgs.follows = "nixpkgs";

  # ---- macos ----
  inputs.nix-darwin.url = "github:LnL7/nix-darwin/master";
  inputs.nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

  # ---- theming ----
  inputs.stylix.url = "github:danth/stylix";
  inputs.minegrub-theme.url = "github:Lxtharia/minegrub-theme";
  #inputs.minesddm.url = "github:Davi-S/sddm-theme-minesddm";
  inputs.minesddm.url = "github:Chucklee1/sddm-theme-minesddm";
  inputs.minesddm.inputs.nixpkgs.follows = "nixpkgs";

  # ---- neovim ----
  inputs.nixvim.url = "github:nix-community/nixvim";
  inputs.nixvim.inputs.nixpkgs.follows = "nixpkgs";

  # ---- wayland ----
  inputs.niri.url = "github:sodiboo/niri-flake";
  inputs.waybar.url = "github:Alexays/Waybar/master";

  # ---- non-flakes ----
  inputs.en_us-dictionary.url = "github:dwyl/english-words";
  inputs.en_us-dictionary.flake = false;

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    # flake helpers
    extlib = import "${self}/libs.nix" {inherit inputs self;};

    # ---- nixvim ----
    nixvim = system:
      inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule {
        inherit system;
        extraSpecialArgs = {inherit inputs;};
        module.imports = extlib.simpleMerge "${self}/nixvim";
      };

    # ---- system  ----
    profiles = {
      desktop = {
        system = "x86_64-linux";
        user = "goat";
      };
      macbook = {
        system = "aarch64-darwin";
        user = "goat";
      };
    };

    mkSystem = machine: cfg: let
      mod = extlib.mergeProfiles "global" machine;
      specialArgs = {
        inherit machine extlib;
        inherit (cfg) system user;
        nixvim = nixvim cfg.system;
      };
    in
      (extlib.builder cfg.system) {
        inherit (cfg) system;
        inherit specialArgs;
        modules =
          mod.nix
          ++ [{home-manager.extraSpecialArgs = specialArgs;}]
          ++ [{_module.args.homeMods = mod.home;}];
      };

    mkSystems = nixpkgs.lib.mapAttrs mkSystem profiles;
  in {
    nixosConfigurations = mkSystems;
    darwinConfigurations = mkSystems;
  };
}
