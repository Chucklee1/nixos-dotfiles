{
  lib,
  pkgs,
  def,
  ...
}: {
  programs = {
    fuzzel.enable = true;
    wlogout = {
      enable = true;
    };
    swaylock = {
      enable = true;
      package = pkgs.swaylock-effects;
    };
  };

  services = {
    dunst.enable = true;
    gnome-keyring.enable = true;
    swayidle = {
      enable = true;
    };
  };
}
