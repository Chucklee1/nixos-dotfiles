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
  inputs.mac-app-util.url = "github:hraban/mac-app-util";

  # ---- theming ----
  inputs.stylix.url = "github:danth/stylix";
  inputs.minegrub-theme.url = "github:Lxtharia/minegrub-theme";
  inputs.minesddm.url = "github:Davi-S/sddm-theme-minesddm";
  inputs.minesddm.inputs.nixpkgs.follows = "nixpkgs";

  # ---- neovim ----
  inputs.nixvim.url = "github:nix-community/nixvim";
  inputs.nixvim.inputs.nixpkgs.follows = "nixpkgs";

  # ---- wayland ----
  inputs.niri.url = "github:sodiboo/niri-flake";
  inputs.waybar. url = "github:Alexays/Waybar/master";

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
        buildType = "nixos";
        homeDir = "/home";
        user = "goat";
      };
      laptop = {
        system = "x86_64-linux";
        builder = nixpkgs.lib.nixosSystem;
        buildType = "nixos";
        homeDir = "/home";
        user = "goat";
      };
      macbook = {
        system = "aarch64-darwin";
        builder = nix-darwin.lib.darwinSystem;
        buildType = "darwin";
        homeDir = "/Users";
        user = "goat";
      };
    };

    mkSystems =
      nixpkgs.lib.mapAttrs
      (machine: cfg: let
        merged = extlib.mergeModules "${self}/modules" {inherit inputs self;};
        mod = extlib.mergeProfiles merged "global" machine;
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
