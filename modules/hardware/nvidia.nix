{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.nvidia;
in {
  options.nvidia = {
    enable = lib.mkEnableOption "enables drivers for nvidia RTX gpus";
    default = false;
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.config.nvidia.acceptLicense = true;
    services.xserver.videoDrivers = ["nvidia"];
    hardware = {
      graphics = {
        enable = true;
        enable32Bit = true;
        extraPackages = with pkgs; [
          vulkan-tools
          vulkan-loader
          nvidia-utils
          libvdpau-va-gl
          ffmpeg
        ];
      };
      nvidia = {
        modesetting.enable = true;
        package = config.boot.kernelPackages.nvidiaPackages.stable;
        forceFullCompositionPipeline = truel
        videoAcceleration = true;
        nvidiaSettings = true;
        open = false;
      };
    };
    environment.variables = {
    LIBVA_DRIVER_NAME = "nvidia";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    __GL_GSYNC_ALLOWED = "1";
    __GL_VRR_ALLOWED = "1";
    __GL_MaxFramesAllowed = "1";
    };
  };
}
