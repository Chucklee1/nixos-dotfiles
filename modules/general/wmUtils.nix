{pkgs, ...}: {
  security = {
    rtkit.enable = true; # rtkit for sound
    polkit.enable = true;
  };
  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = ["gtk"];
  };

  home-manager.sharedModules = [
    {
      home.packages = [pkgs.brightnessctl];
      services = {
        gnome-keyring.enable = true;
        dunst.enable = true;
      };
    }
  ];
}
