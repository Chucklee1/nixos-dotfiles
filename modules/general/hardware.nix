({
  lib,
  config,
  ...
}: {
  options.nvidia.enable = lib.mkEnableOption "enable nvidia drivers";
  config = lib.optionals config.nvidia.enable {
    services.xserver.videoDrivers = ["nvidia"];
    hardware.nvidia = {
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
  };
})
({
  lib,
  config,
  ...
}: {
  options.radeon.enable = lib.mkEnableOption "enable radeon gpu drivers";
  config.services.xserver.videoDrivers = lib.optionals config.radeon.enable ["amd"];
})
({
  lib,
  config,
  ...
}: {
  options.AMD.enable = lib.mkEnableOption "enable amd cpu features ";
  config = lib.optionals config.AMD.enable {
    hardware.cpu.amd.updateMicrocode = true;
    kernelModules = ["kvm-amd"];
  };
})
