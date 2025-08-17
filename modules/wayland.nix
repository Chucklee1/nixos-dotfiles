{
  wayland.nix = [
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

      # polkit n portals
      security.polkit.enable = true;
      xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-gnome
        pkgs.xdg-desktop-portal-gtk
      ];
      xdg.portal.config.common.default = "gnome";
    })
  ];
  wayland.home = [
    ({pkgs, ...}: {
      programs.swaylock.enable = true;
      programs.swaylock.package = pkgs.swaylock-effects;
      programs.waybar.enable = true;
      programs.waybar.systemd.enable = true;
    })
  ];
}
