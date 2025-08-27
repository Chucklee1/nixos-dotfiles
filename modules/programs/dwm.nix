{
  laptop.nix = [
    ({config, pkgs, ...}: {
      services.libinput.enable = true;
      services.libinput.touchpad = {
        naturalScrolling = true;
      };

      services.xserver.enable = true;

      services.xserver.windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs {
          src = ./dwm;
        };
      };
    })
  ];
}
