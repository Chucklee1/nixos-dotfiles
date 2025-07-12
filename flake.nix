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
  inputs.minesddm.url = "github:Chucklee1/sddm-theme-minesddm";
  inputs.minesddm.inputs.nixpkgs.follows = "nixpkgs";

  # ---- neovim ----
  inputs.nix-vim.url = "github:Chucklee1/nix-vim";

  # ---- wayland ----
  inputs.niri.url = "github:sodiboo/niri-flake";
  inputs.waybar.url = "github:Alexays/Waybar/master";

  # ---- python ----
  inputs.poetry2nix.url = "github:nix-community/poetry2nix";
  inputs.baca.url = "github:wustho/baca";
  inputs.baca.flake = false;

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    # ---- additionals ----
    extlib = import ./libs.nix {inherit inputs self;};

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

    mkSystems = nixpkgs.lib.mapAttrs (machine: cfg: let
      builder = extlib.withSystem.ifDarwin cfg.system inputs.nix-darwin.lib.darwinSystem inputs.nixpkgs.lib.nixosSystem;
      mod = extlib.mergeProfiles "global" machine;
      specialArgs = {
        inherit machine;
        inherit (cfg) system user;
        ifSys = {
          linux = A: B:
            extlib.withSystem.ifLinux cfg.system A B;
          darwin = A: B:
            extlib.withSystem.ifDarwin cfg.system A B;
        };
      };
    in
      builder {
        inherit (cfg) system;
        inherit specialArgs;
        modules =
          mod.nix
          ++ [{home-manager.extraSpecialArgs = specialArgs;}]
          ++ [{_module.args.homeMods = mod.home;}];
      })
    profiles;
  in {
    inherit extlib;
    nixosConfigurations = mkSystems;
    darwinConfigurations = mkSystems;
  };
}
