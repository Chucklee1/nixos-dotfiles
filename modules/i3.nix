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
    services = {
      xserver = {
        enable = true;
        windowManager.i3.enable = true;
      };
    };

    home-manager.users.goat = {
      home.packages = with pkgs; [rofi];
    };
  };
}
