{
  description = "Never let them know your next move";

  inputs = {
    # main repos
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    # theming
    stylix.url = "github:danth/stylix";
    minegrub-theme.url = "github:Lxtharia/minegrub-theme";
    minesddm.url = "github:Davi-S/sddm-theme-minesddm";
    minesddm.inputs.nixpkgs.follows = "nixpkgs";
    # neovim
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    # wayland
    niri.url = "github:sodiboo/niri-flake";
    waybar. url = "github:Alexays/Waybar/master";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    # ---- pkgs ----
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      config.allowUnfree = true;
      inherit system;
    };
    nixvimpkgs = inputs.nixvim.legacyPackages.${system};

    # ---- libs & helpers ----
    lib = nixpkgs.lib;
    extlib = import "${self}/libs.nix" {inherit nixpkgs;};

    # ---- nixvim ----
    nixvim = nixvimpkgs.makeNixvimWithModule {
      module.imports = [
        (extlib.mergeModules "${self}/nixvim" {
          inherit lib pkgs inputs;
        })
      ];
    };

    # ---- system  ----
    profiles = ["desktop" "laptop" "umbra"];
    mkMod = host: (extlib.mergeProfiles (extlib.mergeModules "${self}/modules" {
        inherit inputs self system nixvim;
        user = "goat";
        machine = "${host}";
      }) "global"
      host);
  in {
    nixosConfigurations =
      lib.genAttrs profiles
      (host: let
        mod = mkMod host;
      in
        lib.nixosSystem {modules = mod.nix ++ [{_module.args.homeMods = mod.home;}];});
    # devshell mainly for remotes
    devShells.${system}.nixvim = pkgs.mkShell {packages = [nixvim];};
  };
}
