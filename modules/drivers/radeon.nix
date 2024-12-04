{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    radeon.enable = lib.mkEnableOption "enable radeon gpu drivers";
  };

  config = lib.mkIf config.radeon.enable {
    services.xserver.videoDrivers = ["amd"];
  };
}
