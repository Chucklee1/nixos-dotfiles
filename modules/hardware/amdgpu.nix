{
  config,
  lib,
  pkgs,
  ...
}: {
  services.xserver.videoDrivers = ["amdgpu"];
  hardware.amdgpu.amdvlk.enable = true;
  hardware.graphics = {
    enable32Bit = true;
    extraPackages = with pkgs; [
      vulkan-tools
      vulkan-loader
      libvdpau-va-gl
      ffmpeg
    ];
  };
}
