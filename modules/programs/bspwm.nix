{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    bspwm.enable = lib.mkEnableOption "enable bspwm";
  };

  config = lib.mkIf config.bspwm.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          i3
          i3blocks
          rofi # A more flexible alternative to dmenu
          feh # For setting background images
        ];
        #programs.i3blocks.enable = true;
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
