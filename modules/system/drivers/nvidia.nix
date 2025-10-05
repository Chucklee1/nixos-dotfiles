{
  nix = [
    ({config, ...}: {
      nixpkgs.config.nvidia.acceptLicense = true;
      services.xserver.videoDrivers = ["nvidia"];
      hardware.nvidia = {
        package = config.boot.kernelPackages.nvidiaPackages.beta;
        modesetting.enable = true;
        powerManagement.enable = false;
        powerManagement.finegrained = false;
        videoAcceleration = true;
        open = false;
      };
      environment.variables = {
        LIBVA_DRIVER_NAME = "nvidia";
        NVD_BACKEND = "direct";
        GBM_BACKEND = "nvidia-drm";
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      };
    })
  ];
}
