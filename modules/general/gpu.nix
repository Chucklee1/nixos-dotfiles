{
  lib,
  config,
  ...
}: {
  options = {
    radeon.enable = lib.mkEnableOption "enable radeon gpu drivers";
    nvidia.enable = lib.mkEnableOption "enable nvidia drivers";
  };

  config = {
    # gpu drivers for Xorg & Wayland
    services.xserver.videoDrivers =
      lib.optionals config.nvidia.enable ["nvidia"]
      ++ lib.optionals config.radeon.enable ["amd"];

    hardware.nvidia = lib.mkIf config.nvidia.enable {
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
  };
}
