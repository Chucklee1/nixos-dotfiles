{
  self,
  extlib,
  ...
}: let
  inherit (self) inputs;

  builder = {
    nixos = inputs.nixpkgs.lib.nixosSystem;
    darwin = inputs.nix-darwin.lib.darwinSystem;
    home = inputs.home-manager.lib.homeManagerConfiguration;
  };

  root = "${self}/hosts";
  profiles =
    inputs.nixpkgs.lib.concatMapAttrs (file: _: {
      ${extlib.basename file} = import "${root}/${file}" {
        inherit inputs self;
        mod = extlib.readDirRecToAttrset "${self}/modules";
      };
    })
    (builtins.readDir root);
in {
  mkSystems =
    # key = machine, value = cfg
    inputs.nixpkgs.lib.mapAttrs (machine: cfg: let
      # modules
      mod = extlib.loadModulesFromAttrset cfg.modules {
        inherit self inputs extlib;
        target = cfg.type;
      };
      specialArgs = {
        inherit self inputs extlib machine;
        spkgs = import inputs.nixpkgs-stable {inherit (cfg) system;};
        inherit (cfg) system user;
      };
    in
      builder.${cfg.type}
      (
        if cfg.type == "home"
        then {
          pkgs = inputs.nixpkgs.legacyPackages.${cfg.system};
          extraSpecialArgs = specialArgs;
          modules = builtins.concatLists [
            mod.univ
            mod.home
            (cfg.extraConfig or [])
          ];
        }
        else {
          inherit (cfg) system;
          inherit specialArgs;
          modules = builtins.concatLists [
            mod.univ
            mod.nix
            [
              {
                home-manager.users.${cfg.user}.imports = mod.home;
                home-manager.extraSpecialArgs = specialArgs;
              }
            ]
            cfg.extraConfig
          ];
        }
      ))
    profiles;
}
