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
    # nixpkgs
    system = "x86_64-linux";
    lib = nixpkgs.lib;
    pkgs = import nixpkgs {inherit system;};

    # custom
    nixvim' = inputs.nixvim.legacyPackages.${system};
    mylib = import "${self}/lib/merging.nix" {inherit nixpkgs;};
    nixvim = nixvim'.makeNixvimWithModule {
      module = mylib.mergeModules "${self}/nixvim" {
        inherit lib pkgs inputs;
      };
    };

    # ---- system  ----
    profiles = ["desktop" "laptop" "macbook"];
    nixosConfigurations = lib.genAttrs profiles (host:
      lib.nixosSystem {
        modules = let
          mod' = mylib.mergeModules "${self}/modules" {
            # NOTE: SYSTEM CFG ARGS HERE
            inherit inputs self system nixvim;
            user = "goat";
            machine = host;
          };
          mod = mylib.mergeProfiles mod' "global" "${host}";
        in
          mod.nix ++ [{_module.args.homeMods = mod.home;}];
      });
  in {
    inherit nixosConfigurations;
    # devshell mainly for remotes
    packages.${system} = {inherit nixvim;};
    devShells.${system} = {nixvim = pkgs.mkShell {packages = [nixvim];};};
  };
}
