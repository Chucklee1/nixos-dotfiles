{
  description = "Never let them know your next move";

  # ---- main pkg providers ----
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.home-manager.url = "github:nix-community/home-manager";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nur.url = "github:nix-community/NUR";
  inputs.nur.inputs.nixpkgs.follows = "nixpkgs";

  # ---- disk formatting ----
  inputs.disko.url = "github:nix-community/disko";
  inputs.disko.inputs.nixpkgs.follows = "nixpkgs";

  # ---- macos - base ----
  inputs.nix-darwin.url = "github:LnL7/nix-darwin/master";
  inputs.nix-homebrew.url = "github:zhaofengli/nix-homebrew";
  inputs.nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  inputs.mac-app-util.url = "github:hraban/mac-app-util";
  inputs.mac-app-util.inputs.nixpkgs.follows = "nixpkgs";

  # ---- macos - taps ----
  inputs.homebrew-core.url = "github:homebrew/homebrew-core";
  inputs.homebrew-core. flake = false;
  inputs.homebrew-cask.url = "github:homebrew/homebrew-cask";
  inputs.homebrew-cask.flake = false;
  inputs.homebrew-emcasmacport.url = "github:railwaycat/homebrew-emacsmacport";
  inputs.homebrew-emcasmacport.flake = false;

  # ---- theming ----
  inputs.stylix.url = "github:danth/stylix";
  inputs.minegrub-theme.url = "github:Lxtharia/minegrub-theme";
  inputs.minesddm.url = "github:Chucklee1/sddm-theme-minesddm";
  inputs.minesddm.inputs.nixpkgs.follows = "nixpkgs";

  # ---- programs ----
  inputs.nix-vim.url = "github:Chucklee1/nix-vim";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  inputs.niri.url = "github:sodiboo/niri-flake";

  outputs = {self, ...} @ inputs: let
    # ---- additionals ----
    extlib = import ./outputs/libs.nix {inherit inputs self;};
    devShells = extlib.allSystemsWithPkgs (pkgs: import ./outputs/devshells.nix {inherit inputs pkgs;});
    sys = import ./outputs/profiles.nix {inherit inputs self extlib;};
  in {
    #inherit (self) outputs;
    inherit devShells extlib;
    nixosConfigurations = sys.mkSystems sys.profiles;
    darwinConfigurations = sys.mkSystems sys.profiles;
    apps.x86_64-linux.umbra = {
      type = "app";
      program = "${self.nixosConfigurations."umbra".config.system.build.vm}/bin/run-nixos-vm";
    };
    # custom installer iso
    packages.x86_64-linux.installer = self.nixosConfigurations."umbra".config.system.build.isoImage;
  };
}
