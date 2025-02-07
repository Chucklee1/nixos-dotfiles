{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs:
    with nixpkgs.lib; let
      match = flip getAttr;
      read_dir_recursively = dir:
        concatMapAttrs (this:
          match {
            directory = mapAttrs' (subpath: nameValuePair "${this}/${subpath}") (read_dir_recursively "${dir}/${this}");
            regular = {
              ${this} = "${dir}/${this}";
            };
            symlink = {};
          }) (builtins.readDir dir);

      params =
        inputs
        // {
          configs = raw_configs;
          elements = {
            laptop = 7;
            desktop = 8;
            global = 11;
          };
          inherit merge extras;
        };

      read_all_modules = flip pipe [
        read_dir_recursively
        (filterAttrs (n: _: !hasSuffix ".gen.nix" n))
        (mapAttrs (const import))
        (mapAttrs (const (flip toFunction params)))
      ];

      merge = prev: this: {
        nix = prev.nix or [] ++ this.nix or [];
        home = prev.home or [] ++ this.home or [];
      };

      all_modules = attrValues (read_all_modules "${self}/modules");

      raw_configs' = builtins.zipAttrsWith (machine:
        if machine == "extras"
        then mergeAttrsList
        else builtins.foldl' merge {})
      all_modules;

      raw_configs = builtins.removeAttrs raw_configs' ["extras"];

      extras = raw_configs'.extras or {};

      configs = builtins.mapAttrs (const (config:
        nixpkgs.lib.nixosSystem {
          inherit (config) system;
          modules =
            config.nix
            ++ [
              ({
                merge,
                configs,
                ...
              }: {
                laptop = merge configs.global configs.laptop;
                desktop = merge configs.global configs.desktop;
              })
              {_module.args.home = config.home;}
            ];
        }))
      raw_configs;
    in {
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
      nixosConfigurations = builtins.mapAttrs (name: configs.${name}) params.elements;
    };
}
