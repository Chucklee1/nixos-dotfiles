{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    nvidia.enable = lib.mkEnableOption "enable nvidia drivers";
  };

  config = lib.mkIf config.nvidia.enable {
    services.xserver.videoDrivers = ["amd"];
  };
}
