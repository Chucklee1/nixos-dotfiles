{
  lib,
  config,
  pkgs,
  def,
  ...
}: let
  mkOptions = attrs:
    lib.genAttrs attrs (name:
      lib.mkOption {
        type = lib.types.listOf lib.types.attrs;
        default = [];
        description = "Configuration for ${name}.";
      });
in {
  imports = [
    ./dwm.mod.nix
    ./software.mod.nix
    ./system.mod.nix
    ./theming.mod.nix
    ./hardware.mod.nix
  ];
  options = mkOptions [
    "nixMod.global"
    "nixMod.laptop"
    "nixMod.desktop"
    "homeMod.global"
    "homeMod.desktop"
  ];

  config = {
    nixosModules =
      lib.concatMapAttrs (_: modules: modules) config.nixMod.global
      ++ lib.concatMapAttrs (_: modules: modules) config.nixMod.${def.host};

    homeModules =
      lib.concatMapAttrs (_: modules: modules) config.homeMod.global
      ++ lib.concatMapAttrs (_: modules: modules) config.homeMod.${def.host};
  };
}
