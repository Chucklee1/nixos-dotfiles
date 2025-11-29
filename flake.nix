{
  description = "Never let them know your next move";
  inputs = {
    # ---- main pkg providers ----
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-25.05";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";

    # ---- disk formatting ----
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    # ---- macos - base ----
    nix-darwin.url = "github:LnL7/nix-darwin/master";
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # ---- macos - taps ----
    homebrew-core.url = "github:homebrew/homebrew-core";
    homebrew-core.flake = false;
    homebrew-cask.url = "github:homebrew/homebrew-cask";
    homebrew-cask.flake = false;
    homebrew-bundle.url = "github:homebrew/homebrew-bundle";
    homebrew-bundle.flake = false;

    # ---- theming ----
    stylix.url = "github:danth/stylix";
    minegrub-theme.url = "github:Lxtharia/minegrub-theme";
    minesddm.url = "github:Davi-S/sddm-theme-minesddm/development";

    # ---- nixvim ----
    nixvim.url = "github:nix-community/nixvim";
    en_us-dictionary.url = "github:dwyl/english-words";
    en_us-dictionary.flake = false;

    # ---- emacs ----
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # ---- window managers ----
    dwm.url = "github:Chucklee1/dwm";
    dwm.flake = false;
    niri.url = "github:sodiboo/niri-flake";
  };

  outputs = {self, ...} @ inputs: let

    # ---- imports - flake ----
    extlib = import ./flake/libs.nix {inherit self;};
    sys = import ./flake/profiles.nix {inherit self extlib;};

    # ---- imports - pkgs ----
    nixvim = import ./pkgs/nixvim {inherit self;};
    emacs = import ./pkgs/emacs;
  in {
    inherit extlib;

    nixosConfigurations = sys.mkSystems sys.profiles;
    darwinConfigurations = sys.mkSystems sys.profiles;

    devShells = extlib.allSystemsWithPkgs (pkgs:
      import ./flake/devshells.nix {inherit inputs pkgs;}
    );

    apps = extlib.allSystems (system: {
      umbra = {
        type = "app";
        program = "${self.nixosConfigurations."umbra".config.system.build.vm}/bin/run-nixos-vm";
      };
    });

    packages = extlib.allSystems (system: {
      # custom installer iso
      installer = self.nixosConfigurations."umbra".config.system.build.isoImage;
      nixvim = nixvim.package {inherit system;};
    });

    overlays = {
      inherit emacs;
      nixvim = nixvim.overlay;
    };
  };
}
