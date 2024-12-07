{
  lib,
  config,
  ...
}: {
  config = lib.mkIf (config.niri.enable && config.nvidia.enable) {
    hardware.nvidia.modesetting.enable = true;
    home-manager.sharedModules = [
      {
        programs.niri.settings.environment = {
          WLR_NO_HARDWARE_CURSORS = "1";
          GBM_BACKEND = "nvidia_drm";
          __GLX_VENDOR_LIBRARY_NAME = "nvidia";
          LIBVA_DRIVER_NAME = "nvidia";
        };
      }
    ];
  };
}
