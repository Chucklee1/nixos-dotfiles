{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    nvidia.enable = lib.mkEnableOption "enable nvidia drivers";
  };

  config = lib.mkIf config.nvidia.enable {
    # temp
    hardware.nvidia.modesetting.enable = true;
    environment.variables = {
      WLR_NO_HARDWARE_CURSORS = "1";
      GBM_BACKEND = "nvidia_drm";
      __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      LIBVA_DRIVER_NAME = "nvidia";
    };
    # end temp
    # nvidia driver for Xorg & Wayland
    services.xserver.videoDrivers = ["nvidia"];
    hardware.nvidia = {
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
  };
}
