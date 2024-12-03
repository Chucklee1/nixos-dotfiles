{pkgs, ...}: {
  services.xserver.enable = true;
  services.xserver.desktopManager.budgie.enable = true;
}
