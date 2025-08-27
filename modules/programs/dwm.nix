{
  laptop.nix = [
    ({config, ...}: {
      services.libinput.touchpad = {
        enable = true;
        naturalScrolling = true;
      };

      services.xserver.enable = true;

      services.xserver.windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs {
          src = "${config.users.users.goat.home}/Repos/dwm";
        };
      };
    })
  ];
}
