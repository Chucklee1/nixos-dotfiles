let
  nixpkgs = import <nixpkgs> {};
  lib = nixpkgs.lib;
in
  with lib; rec {
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
          desktop = 8;
          laptop = 11;
          global = 77;
        };
        inherit merge;
      };

    read_all_modules = flip pipe [
      read_dir_recursively
      (filterAttrs (flip (const (hasSuffix ".mod.nix"))))
      (mapAttrs (const import))
      (mapAttrs (const (flip toFunction params)))
    ];

    merge = prev: this:
      {
        modules = prev.modules or [] ++ this.modules or [];
        home_modules = prev.home_modules or [] ++ this.home_modules or [];
      }
      // (optionalAttrs (prev ? system || this ? system) {
        system = prev.system or this.system;
      });

    all_modules = attrValues (read_all_modules "${self}");

    raw_configs' =
      builtins.zipAttrsWith (builtins.foldl' merge {})
      all_modules;

    configs = builtins.mapAttrs (const (config:
      nixpkgs.lib.nixosSystem {
        inherit (config) system;
        modules =
          config.modules
          ++ [
            {
              _module.args.home_modules = config.home_modules;
            }
          ];
      }))
    raw_configs;
  }
