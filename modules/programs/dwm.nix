{inputs, ...}: {
  nix = [
    ({pkgs, ...}: {
      services.xserver.windowManager.dwm = {
        enable = true;
        package = pkgs.dwm.overrideAttrs {src = inputs.dwm;};
      };
    })
  ];
}
