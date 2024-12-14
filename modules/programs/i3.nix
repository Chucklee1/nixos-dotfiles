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
        home.packages = with pkgs; [
          i3
          i3blocks
          rofi # A more flexible alternative to dmenu
          feh # For setting background images
        ];
        #programs.i3blocks.enable = true;
        #xsession = {
        #  enable = true;
        #  windowManager.i3 = {
        #    enable = true;
        #    config = {
        #      terminal = "kitty";};
        #  };
        #};
      }
    ];
  };
}
