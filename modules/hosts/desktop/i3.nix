_: {
  services.xserver = {
    enable = true;
    windowManager.i3.enable = true;
    config = {
      Monitor = {
        "DP-2" = {
          Identifier = "<MonitorIdentifier>";
          Option = "PreferredMode" "1920x1080@165";
        };
      };
    };
  };

  home-manager.sharedModules = [
    {
      xsession.enable = true;
      xsession.windowManager.i3 = {
        enable = true;
        package = pkgs.i3-gaps;
        config = {
          modifier = "mod4";
          gaps = {
            inner = 5;
            outer = 5;
          };
          keybindings = let
            modifier = config.xsession.windowManager.i3.config.modifier;
          in
            lib.mkOptionDefault {
              "${modifier}+Return" = "exec kitty";
              "${modifier}+q" = "kill";
              "${modifier}+d" = "exec ${pkgs.rofi}/bin/rofi -show drun";
            };
        };
      };
    }
  ];
}
