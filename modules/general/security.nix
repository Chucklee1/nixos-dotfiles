{pkgs, ...}: {
  services.gnome.gnome-keyring.enable = true;
  security = {
    rtkit.enable = true; # rtkit for sound
    polkit.enable = true;
  };
}
