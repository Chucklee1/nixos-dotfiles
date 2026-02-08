{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        egl-wayland
        qt5.qtwayland
        qt6.qtwayland
        brightnessctl
        wev
        wmenu
        swaynotificationcenter
        xwayland
        xwayland-run
        wl-color-picker
        wl-clipboard
      ];
    })
  ];
  home = [
    ({pkgs, ...}: {
      programs.swaylock.enable = true;
      programs.swaylock.package = pkgs.swaylock-effects;
    })
  ];
}
