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
    nix-flatpak.url = "github:gmodena/nix-flatpak/?ref=latest";
    nix-cachyos-kernel.url = "github:xddxdd/nix-cachyos-kernel/release";

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
    quickshell.url = "git+https://git.outfoxxed.me/outfoxxed/quickshell";
    quickshell.inputs.nixpkgs.follows = "nixpkgs";
    qml-niri.url = "github:imiric/qml-niri/main";
    qml-niri.inputs.nixpkgs.follows = "nixpkgs";
    qml-niri.inputs.quickshell.follows = "quickshell";

    # ---- lang support ----
    fenix.url = "github:nix-community/fenix";
    fenix.inputs.nixpkgs.follows = "nixpkgs";

    # ---- editors ----
    nixvim.url = "github:nix-community/nixvim";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    en_us-dictionary.url = "github:dwyl/english-words";
    en_us-dictionary.flake = false;

    # ---- window managers ----
    dwm.url = "github:Chucklee1/dwm";
    dwm.flake = false;
    niri.url = "github:sodiboo/niri-flake";

    # ---- Apps ----
    prismlauncher.url = "github:PrismLauncher/PrismLauncher";
    prismlauncher.inputs.nixpkgs.follows = "nixpkgs";
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
    zen-browser.inputs.nixpkgs.follows = "nixpkgs";
    zen-browser.inputs.home-manager.follows = "home-manager";
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

    nixosConfigurations = sys.mkSystems;
    darwinConfigurations = sys.mkSystems;

    devShells = extlib.allSystemsWithPkgs (
      pkgs:
        import ./flake/devshells.nix {inherit inputs pkgs;}
    );

    packages = extlib.allSystems (system: {
      nixvim = nixvim.package system;
    });

    overlays = {
      inherit emacs;
      nixvim = nixvim.overlay;
    };
  };
}
