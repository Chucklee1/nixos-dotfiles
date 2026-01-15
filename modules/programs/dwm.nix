{inputs, ...}: {
  nix = [
    ({config, pkgs, ...}: {
      services.xserver.windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs {src = inputs.dwm;};
        extraSessionCommands = ''
          ${pkgs.feh}/bin/feh --bg-scale ${config.stylix.image} &
        '';
      };
    })
  ];
}
