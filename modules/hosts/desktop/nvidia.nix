{
  lib,
  pkgs,
  config,
  ...
}: let
  cfg = config.nvidia;
  nvidiaPackage = config.boot.kernelPackages.nvidiaPackages.latest;
in {
  options.nvidia = {enable = lib.mkEnableOption "nvidia drivers";};

  config = lib.mkIf cfg.enable {
    nixpkgs.config.nvidia.acceptLicense = true;
    services.xserver.videoDrivers = ["nvidia"];

    environment = {
      variables = {
        GBM_BACKEND = "nvidia-drm";
        LIBVA_DRIVER_NAME = "nvidia";
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      };
      systemPackages = with pkgs; [
        vulkan-loader
        vulkan-validation-layers
        vulkan-tools
      ];
    };

    hardware = {
      nvidia = {
        modesetting.enable = true;
        package = nvidiaPackage;
        powerManagement.enable = false;
        powerManagement.finegrained = false;
        open = false;
      };
      graphics. extraPackages = with pkgs; [
        nvidia-vaapi-driver
        vaapiVdpau
        libvdpau-va-gl
      ];
    };
  };
}
