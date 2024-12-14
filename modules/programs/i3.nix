{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    i3.enable = lib.mkEnableOption "enable i3 wm";
  };

  config = lib.mkIf config.i3.enable {
    home-manager.sharedModules = [
      {
        xsession = {
          enable = true;
          windowManager.i3 = {
            enable = true;
          };
        };
      }
    ];
  };
}
