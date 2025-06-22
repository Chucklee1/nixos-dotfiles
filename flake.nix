{
  description = "Never let them know your next move";

  inputs = {
    # main repos
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # disk formatting
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    # macos
    nix-darwin.url = "github:LnL7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    mac-app-util.url = "github:hraban/mac-app-util";
    # theming
    stylix.url = "github:danth/stylix";
    minegrub-theme.url = "github:Lxtharia/minegrub-theme";
    minesddm.url = "github:Davi-S/sddm-theme-minesddm";
    minesddm.inputs.nixpkgs.follows = "nixpkgs";
    # neovim
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    # wayland
    niri.url = "github:sodiboo/niri-flake";
    waybar. url = "github:Alexays/Waybar/master";
  };

  outputs = {
    self,
    nixpkgs,
    nix-darwin,
    ...
  } @ inputs: let
    # ---- libs & helpers ----
    extlib = import "${self}/libs.nix" {inherit nixpkgs;};

    # ---- nixvim ----
    nixvim = system:
      inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule {
        inherit system;
        module.imports = extlib.simpleMerge "${self}/nixvim";
      };

    # ---- system  ----
    profiles = {
      desktop = {
        system = "x86_64-linux";
        builder = nixpkgs.lib.nixosSystem;
        user = "goat";
      };
      laptop = {
        system = "x86_64-linux";
        builder = nixpkgs.lib.nixosSystem;
        user = "goat";
      };
      macbook = {
        system = "aarch64-darwin";
        builder = nix-darwin.lib.darwinSystem;
        user = "goat";
      };
    };

    merged = extlib.mergeModules "${self}/modules" {inherit inputs self;};
    mkMod = host: (extlib.mergeProfiles merged "global" host);

    mkSystems =
      nixpkgs.lib.mapAttrs
      (machine: cfg: let
        mod = mkMod machine;
        specialArgs = {
          inherit machine;
          inherit (cfg) system user;
          nixvim = nixvim cfg.system;
        };
      in
        cfg.builder {
          inherit (cfg) system;
          inherit specialArgs;
          modules =
            mod.nix
            ++ [{home-manager.extraSpecialArgs = specialArgs;}]
            ++ [{_module.args.homeMods = mod.home;}];
        })
      profiles;
  in {
    nixosConfigurations = mkSystems;
    darwinConfigurations = mkSystems;
  };
}
