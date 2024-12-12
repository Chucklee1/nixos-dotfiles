{pkgs, ...}: {
  services.gnome.gnome-keyring.enable = true;
  security = {
    rtkit.enable = true; # rtkit for sound
    polkit.enable = true;
  };
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };
}
