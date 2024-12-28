{
  lib,
  pkgs,
  config,
  ...
}: let
  nvidiaPackage = config.boot.kernelPackages.nvidiaPackages.latest;
in {
  nixpkgs.config.nvidia.acceptLicense = true;
  services.xserver.videoDrivers = ["nvidia"];

  environment.systemPackages = with pkgs; [
      vulkan-loader
      vulkan-validation-layers
      vulkan-tools
    ];

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
}
