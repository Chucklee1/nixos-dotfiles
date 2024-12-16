{
  lib,
  config,
  ...
}: let
  nvidia_bool = lib.mkIf config.nvidia.enable;
  nvidia_opt = lib.optionals config.nvidia.enable;
  radeon_opt = lib.optionals config.radeon.enable;
  AMD_bool = lib.mkIf config.AMD.enable;
  AMD_opt = lib.optionals config.AMD.enable;
in {
  options = {
    nvidia.enable = lib.mkEnableOption "enable nvidia drivers";
    radeon.enable = lib.mkEnableOption "enable radeon gpu drivers";
    AMD.enable = lib.mkEnableOption "enable amd cpu features ";
  };

  config = {
    # GPU
    services.xserver.videoDrivers = nvidia_opt ["nvidia"] ++ radeon_opt ["radeon"];
    hardware.nvidia = nvidia_opt {
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };

    # CPU
    hardware.cpu.amd.updateMicrocode = AMD_bool true;
    boot.kernelModules = AMD_opt ["kvm-amd"];
  };
}
