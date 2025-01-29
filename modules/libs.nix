{
  lib,
  config,
  ...
}: {
  _module.args.mk = {
    conf = opt: lib.mkIf config.${opt}.enable;
    opt = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "read";
    };
  };
}
