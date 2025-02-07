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

      # `const` helper function is used extensively: the function is constant in regards to the name of the attribute.

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

      # It is important to note, that when adding a new `.mod.nix` file, you need to run `git add` on the file.
      # If you don't, the file will not be included in the flake, and the modules defined within will not be loaded.

      read_all_modules = flip pipe [
        read_dir_recursively
        (filterAttrs (n: _: !hasSuffix ".gen.nix" n))
        (mapAttrs (const import))
        (mapAttrs (const (flip toFunction params)))
      ];

      merge = prev: this:
        {
          modules = prev.nix or [] ++ this.nix or [];
          home = prev.home or [] ++ this.home or [];
        }
        // (optionalAttrs (prev ? system || this ? system) {
          system = prev.system or this.system;
        });

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
          system = "x86_64-linux";
          modules =
            config.modules
            ++ [
              {
                _module.args.home = config.home;
              }
            ];
        }))
      raw_configs;
    in {
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
      nixosConfigurations = builtins.mapAttrs (name: const configs.${name}) params.elements;
    };
}
