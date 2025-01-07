{
  pkgs,
  inputs,
  ...
}: {
  nixpkgs.overlays = [inputs.niri.overlays.niri];
  programs.niri = {
    enable = true;
    package = pkgs.niri-unstable;
  };

  environment.systemPackages = with pkgs; [
    egl-wayland
    qt5.qtwayland
    qt6.qtwayland
    wl-clipboard
    wev
    xwayland
    xwayland-run
  ];
}
