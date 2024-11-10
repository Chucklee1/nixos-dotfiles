{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    radeon.enable = lib.mkEnableOption "enable radeon drivers";
  };

  config = lib.mkIf config.radeon.enable {
    boot.initrd.kernelModules = ["amdgpu"];
    services.xserver.videoDrivers = ["amdgpu"];
  };
}
