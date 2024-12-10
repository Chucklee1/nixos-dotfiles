{
  lib,
  config,
  ...
}: {
  options = {
    nvidia.enable = lib.mkEnableOption "enable nvidia drivers";
    radeon.enable = lib.mkEnableOption "enable radeon gpu drivers";
    AMD.enable = lib.mkEnableOption "enable amd cpu features ";
  };

  config = {
    # radeon
    services.xserver.videoDrivers =
      lib.optionals config.nvidia.enable ["amd"]
      # nvidia
      ++ config.radeon.enable ["nvidia"];
    hardware.nvidia = lib.optionals config.nvidia.enable {
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };

    # AMD
    hardware.cpu.amd.updateMicrocode = lib.optionals config.AMD.enable;
    kernelModules = lib.optionals config.AMD.enable ["kvm-amd"];
  };
}
