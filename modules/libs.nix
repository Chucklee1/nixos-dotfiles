{lib, ...}: {
  opt = mod: (lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = "enable ${mod} module";
  });
}
