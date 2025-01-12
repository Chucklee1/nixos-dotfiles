{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.amdgpu;
in {
  options.amdgpu = {
    enable = lib.mkEnableOption "enable drivers for amgpu and video acceleration";
  };

  config = lib.mkIf cfg.enable {
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
