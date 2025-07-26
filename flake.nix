{
  description = "Never let them know your next move";

  # ---- main pkg providers ----
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.home-manager.url = "github:nix-community/home-manager";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";

  # ---- disk formatting ----
  inputs.disko.url = "github:nix-community/disko";
  inputs.disko.inputs.nixpkgs.follows = "nixpkgs";

  # ---- emphereal system ----
  inputs.impermanence.url = "github:nix-community/impermanence";

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
    nix-darwin,
    ...
  } @ inputs: let
    # ---- additionals ----
    extlib = import ./outputs/libs.nix {inherit inputs self;};
    devShells = extlib.allSystemsWithPkgs (
      pkgs: import ./outputs/devshells.nix {inherit inputs pkgs;}
    );

    # ---- system  ----
    profiles = let
      mod = extlib.loadModules "${self}/modules" {inherit inputs self;};
      mapMods = mods: {
        nix = builtins.concatLists (map (m: m.nix) mods);
        home = builtins.concatLists (map (m: m.home) mods);
      };
    in {
      nixos = {
        desktop = {
          system = "x86_64-linux";
          modules = mapMods [mod.global mod.desktop];
          user = "goat";
        };
        laptop = {
          system = "x86_64-linux";
          modules = mapMods [mod.global mod.laptop mod.wayland];
          user = "goat";
        };
        umbra = {
          system = "x86_64-linux";
          modules = mapMods [mod.umbra];
          user = "nixos";
        };
      };
      darwin.macbook = {
        system = "aarch64-darwin";
        modules = mapMods [mod.global mod.macbook];
        user = "goat";
      };
    };

    mkSystems = cfgs:
      nixpkgs.lib.mapAttrs (machine: cfg: let
        builder =
          if cfgs ? darwin
          then nix-darwin.lib.darwinSystem
          else nixpkgs.lib.nixosSystem;
        specialArgs = {
          inherit machine;
          inherit (cfg) system user;
          MOL = A: B: extlib.withSystem.ifDarwinElseLinux cfg.system A B;
        };
      in
        builder {
          inherit (cfg) system;
          inherit specialArgs;
          modules =
            builtins.concatLists
            [
              cfg.modules.nix
              [{_module.args.homeMods = cfg.modules.home;}]
              [{home-manager.extraSpecialArgs = specialArgs;}]
            ];
        })
      cfgs;
  in rec {
    #inherit (self) outputs;
    inherit devShells extlib;
    nixosConfigurations = mkSystems profiles.nixos;
    darwinConfigurations = mkSystems profiles.darwin;
    apps.x86_64-linux.umbra = {
      type = "app";
      program = "${nixosConfigurations."umbra".config.system.build.vm}/bin/run-nixos-vm";
    };
  };
}
