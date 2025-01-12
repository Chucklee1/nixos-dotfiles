{
  config,
  lib,
  pkgs,
  host,
  ...
}: {
  config =
    lib.mkIf (host == "laptop") {
      services.xserver.videoDrivers = ["amdgpu"];
      hardware.amdgpu.amdvlk = {
        enable = true;
        support32Bit = true;
      };
      hardware.graphics = {
        enable32Bit = true;
        extraPackages = with pkgs; [
          vulkan-tools
          vulkan-loader
          amdvlk
          libvdpau-va-gl
          ffmpeg
        ];
      };
    };
}
