{
  description = "i dont kow what im doing";

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
    minecraft-plymouth = {
      url = "github:nikp123/minecraft-plymouth-theme";
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
    mylib = import "${self}/lib" {inherit nixpkgs;};

    # ---- nixvim ----
    nixvim = nixvimpkgs.makeNixvimWithModule {
      module = mylib.mergeModules "${self}/nixvim" {
        inherit lib pkgs inputs;
      };
    };

    # ---- system  ----
    metal = host:
      lib.pipe [
        (mylib.mergeModules "${self}/modules" {
          # NOTE: SYSTEM CFG ARGS HERE
          inherit inputs self system nixvim;
          host = "goat";
          machine = "${host}";
        })
        (out: mylib.mergeProfiles out."global" out."${host}")
      ];
    profiles = {
      desktop = metal "desktop";
      macbook = metal "macbook";
      umbra = "umbra";
    };
  in {
    nixosConfigurations =
      lib.genAttrs
      (lib.attrNames profiles)
      (host:
        lib.nixosSystem {
          modules = profiles.${host}.nix ++ [{_module.args.homeMods = profiles.${host}.home;}];
        });
    # devshell mainly for remotes
    packages.${system} = {inherit nixvim;};
    devShells.${system} = mylib.wrapPkgInShell nixvim;
  };
}
