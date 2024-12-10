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
    # GPU
    services.xserver.videoDrivers = lib.optionals config.nvidia.enable ["nvidia"] ++ lib.optionals config.radeon.enable ["radeon"];
    hardware.nvidia = lib.mkIf config.nvidia.enable {
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };

    # CPU
    hardware.cpu.amd.updateMicrocode = lib.mkIf config.AMD.enable true;
    boot.kernelModules = lib.mkIf config.AMD.enable ["kvm-amd"];
  };
}
