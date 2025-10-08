{
  description = "Never let them know your next move";

  # ---- main pkg providers ----
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-25.05";
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

  # ---- macos - taps ----
  inputs.homebrew-core.url = "github:homebrew/homebrew-core";
  inputs.homebrew-core.flake = false;
  inputs.homebrew-cask.url = "github:homebrew/homebrew-cask";
  inputs.homebrew-cask.flake = false;

  # ---- theming ----
  inputs.stylix.url = "github:danth/stylix";
  inputs.minegrub-theme.url = "github:Lxtharia/minegrub-theme";
  inputs.minesddm.url = "github:Davi-S/sddm-theme-minesddm/development";

  # ---- nixvim ----
  inputs.nixvim.url = "github:nix-community/nixvim";
  inputs.en_us-dictionary.url = "github:dwyl/english-words";
  inputs.en_us-dictionary.flake = false;

  # ---- emacs ----
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  inputs.org-modern-indent.url = "github:jdtsmith/org-modern-indent";
  inputs.org-modern-indent.flake = false;

  # ---- programs ----
  inputs.dwm.url = "github:Chucklee1/dwm";
  inputs.dwm.flake = false;
  inputs.niri.url = "github:sodiboo/niri-flake";
  inputs.zen-browser.url = "github:0xc000022070/zen-browser-flake";
  inputs.zen-browser.inputs.nixpkgs.follows = "nixpkgs";

  outputs = {self, ...} @ inputs: let
    # ---- additionals ----
    extlib = import ./outputs/libs.nix {inherit inputs self;};
    devShells = extlib.allSystemsWithPkgs (pkgs: import ./outputs/devshells.nix {inherit inputs pkgs;});
    nvlib = import ./outputs/nixvim.nix {inherit self;};
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

    packages = extlib.allSystems (system: {
      # custom installer iso
      installer = self.nixosConfigurations."umbra".config.system.build.isoImage;
      nixvim = {
        core = nvlib.mkModule system "core";
        full = nvlib.mkModule system "full";
      };
    });
    overlays.default = self: prev: {
      nixvim.core = nvlib.mkModule self.system "core";
      nixvim.full = nvlib.mkModule self.system "full";
    };
  };
}
