{
  config,
  lib,
  ...
}: {
  options = {
    nvidia-wayland.enable = lib.mkEnableOption "nvidia wayland compatability patch";
  };

  config = lib.mkIf config.nvidia-wayland.enable {
    environment.variables = {
      WLR_NO_HARDWARE_CURSORS = "1";
      GBM_BACKEND = "nvidia_drm";
      __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      LIBVA_DRIVER_NAME = "nvidia";
    };
  };
}
