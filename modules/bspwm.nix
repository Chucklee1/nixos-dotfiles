{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {
    bspwm.enable = lib.mkEnableOption "enable bspwm window manager module";
  };

  config = lib.mkIf config.bspwm.enable {
    xserver.enable = true;
    environment.systemPackages = with pkgs; [
      feh
      xorg.xrandr
    ];
    home-manager.users.goat = {
      xsession.enable = true;
      xsession.windowManager = {
        bspwm.enable = true;
        sxhkd.package = pkgs.sxhkd;
        configFile = "/home/your_user/.config/bspwm/bspwmrc";
        sxhkd.configFile = "/home/your_user/.config/sxhkd/sxhkdrc";
      };
      programs.rofi.enable = true;
      services = {
        polybar.enable = true;
        dunst.enable = true;
        redshift.enable = true;
        picom.enable = true;
      };
    };
  };
}
