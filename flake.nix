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

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    # ---- additionals ----
    extlib = import ./outputs/libs.nix {inherit inputs self;};
    devShells = extlib.allSystemsWithPkgs (
      pkgs: import ./outputs/devshells.nix {inherit pkgs;}
    );

    # ---- system  ----
    profiles = {
      desktop = {
        system = "x86_64-linux";
        builder = inputs.nixpkgs.lib.nixosSystem;
        mod = extlib.mergeProfiles "global" "desktop";
        user = "goat";
      };
      laptop = {
        system = "x86_64-linux";
        builder = inputs.nixpkgs.lib.nixosSystem;
        mod = extlib.mergeProfiles "global" "laptop";
        user = "goat";
      };
      umbra = {
        system = "x86_64-linux";
        builder = inputs.nixpkgs.lib.nixosSystem;
        mod = extlib.mergeProfiles "umbra" "umbra";
        user = "umbra";
      };
      macbook = {
        system = "aarch64-darwin";
        builder = inputs.nix-darwin.lib.darwinSystem;
        mod = extlib.mergeProfiles "global" "macbook";
        user = "goat";
      };
    };

    mkSystems = nixpkgs.lib.mapAttrs (machine: cfg: let
      inherit (cfg) builder mod;
      metal = list:
        if machine == "umbra"
        then []
        else list;
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
          ++ (metal [{home-manager.extraSpecialArgs = specialArgs;}])
          ++ (metal [{_module.args.homeMods = mod.home;}]);
      })
    profiles;
  in rec {
    inherit devShells extlib;
    nixosConfigurations = mkSystems;
    darwinConfigurations = mkSystems;
    apps.x86_64-linux.umbra = {
      type = "app";
      program = "${nixosConfigurations."umbra".config.system.build.vm}/bin/run-nixos-vm";
    };
  };
}
