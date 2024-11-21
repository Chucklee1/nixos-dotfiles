{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {hyprland.enable = lib.mkEnableOption "enable hyprland window manager module";};

  config = lib.mkIf config.hyprland.enable {
    programs.hyprland = {
      xwayland.enable = true;
      enable = true; 
      portalPackage = pkgs.xdg-desktop-portal-hyprland;
    };
    home-manager.users.goat.stylix.targets.hyprland.enable = true;
    environment.systemPackages = pkgs.hyprland-protocols;
  };
}
