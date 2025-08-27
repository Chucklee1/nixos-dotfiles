{inputs, ...}: {
  dwm.nix = [
    ({lib, config, pkgs, ...}: {
      services.libinput.enable = true;
      services.libinput.touchpad = {
        naturalScrolling = true;
      };

      services.xserver.enable = true;

      services.xserver.windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs {src = inputs.dwm;};
        extraSessionCommands = ''
          ${pkgs.feh}/bin/feh --bg-scale ${config.stylix.image} &
          ${pkgs.picom}/bin/picom --backend egl &
        '';
      };
    })
  ];
  dwm.home = [
    ({pkgs, ...}: {home.packages = with pkgs; [dmenu];})
  ];
}
