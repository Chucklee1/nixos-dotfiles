{self, extlib, ...}: let
  inherit (self) inputs;
in {
  profiles =  let
    mod = extlib.readDirRecToAttrset "${self}/modules";
    root = "${self}/hosts";
  in inputs.nixpkgs.lib.concatMapAttrs (file: _: {
    ${extlib.basename file} =
      import "${root}/${file}" {inherit mod inputs;};
  })
    (builtins.readDir root);

  mkSystems = cfgs:
    # key = machine, value = cfg
    inputs.nixpkgs.lib.mapAttrs (machine: cfg: let
      # modules
      mod = extlib.loadModulesFromAttrset cfg.modules {inherit self inputs extlib;};
      specialArgs = {
        inherit self inputs extlib machine;
        spkgs = import inputs.nixpkgs-stable {inherit (cfg) system;};
        inherit (cfg) system user;
      };
    in
      cfg.builder {
        inherit (cfg) system;
        inherit specialArgs;
        modules =
          builtins.concatLists
            [
              mod.nix
              [{_module.args.homeMods = mod.home;}]
              [{home-manager.extraSpecialArgs = specialArgs;}]
              cfg.extraConfig
            ];
      })
      cfgs;
}
