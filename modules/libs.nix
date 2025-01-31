{
  lib,
  modules,
  ...
}: {
  opts = lib.attrsets.genAttrs modules (mod: {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "enables ${mod} module";
    };
  });
}
