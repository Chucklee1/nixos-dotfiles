{
  description = "Never let them know your next move";

  inputs = {
    # main repos
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # theming
    stylix = {
      url = "github:danth/stylix";
    };
    minegrub-theme = {
      url = "github:Lxtharia/minegrub-theme";
    };
    minesddm = {
      url = "github:Davi-S/sddm-theme-minesddm";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # neovim
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # wayland
    niri = {
      url = "github:sodiboo/niri-flake";
    };
    waybar = {
      url = "github:Alexays/Waybar/master";
    };
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    # ---- pkgs ----
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
    nixvimpkgs = inputs.nixvim.legacyPackages.${system};

    # ---- libs & helpers ----
    lib = nixpkgs.lib;
    mylib = import "${self}/libs.nix" {inherit nixpkgs;};

    # ---- nixvim ----
    nixvim = nixvimpkgs.makeNixvimWithModule {
      module.imports = [
        (mylib.mergeModules "${self}/nixvim" {
          inherit lib pkgs inputs;
        })
      ];
    };

    # ---- system  ----
    metal = host: (mylib.mergeModules "${self}/modules" {
      # NOTE: SYSTEM CFG ARGS HERE
      inherit inputs self system nixvim;
      user = "goat";
      machine = "${host}";
    });
    profiles = {
      desktop = mylib.mergeProfiles (metal "desktop") "global" "desktop";
      macbook = mylib.mergeProfiles (metal "macbook") "global" "macbook";
    };
  in {
    nixosConfigurations =
      lib.genAttrs
      (lib.attrNames profiles)
      (host: let
        mod = profiles.${host};
      in
        lib.nixosSystem {
          modules = mod.nix ++ [{_module.args.homeMods = mod.home;}];
        });
    # devshell mainly for remotes
    devShells.${system}.nixvim = pkgs.mkShell {packages = [nixvim];};
  };
}
