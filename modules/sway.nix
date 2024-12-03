{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    sway.enable = lib.mkEnableOption "enable sway wm";
  };

  config = lib.mkIf config.sway.enable {
    programs.sway.enable = true;
  };
}
