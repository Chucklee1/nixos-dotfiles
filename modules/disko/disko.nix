{inputs, ...}: {
  nix = [
    inputs.disko.nixosModules.default
    ({lib, ...}: {
      options.opts.disko.device = lib.mkOption {
        description = "Device name you wish to partition (use /dev)";
        type = lib.types.str;
        default = "none";
      };
    })
  ];
}
