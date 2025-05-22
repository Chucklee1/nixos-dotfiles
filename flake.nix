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
      # using pull req fork until hot-corner feature is merged into main branch
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
      module = mylib.mergeModules "${self}/nixvim" {
        inherit lib pkgs inputs;
      };
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
      umbra = "umbra";
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
    packages.${system} = {inherit nixvim;};
    devShells.${system} = {
      nixvim = pkgs.mkShell {packages = [nixvim];};
      gnu = pkgs.mkShell {
        packages = with pkgs; [
          gnumake
          gdb
          gcc
        ];
        shellHook = ''
          export ROOT="$PWD/.nix-env"
          export HOME="$ROOT/home"
          export XDG_CACHE_HOME="$ROOT/cache"
          export TMPDIR="$ROOT/tmp"
          mkdir -p $HOME $XDG_CACHE_HOME $TMPDIR
        '';
      };
    };
  };
}
