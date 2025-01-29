{
  lib,
  config,
  ...
}:
/*
import under mk: (import ./libs.nix) {inherit lib config;};
*/
{
  conf = opt: lib.mkIf config.${opt}.enable;
  opt = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = "read";
  };
}
