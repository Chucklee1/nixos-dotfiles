{
  pkgs,
  lib,
  config,
  ...
}: {
  options = {hyprland.enable = lib.mkEnableOption "enable hyprland window manager module";};

  config = lib.mkIf config.hyprland.enable {
    # system hyprland 
    programs.hyprland = {
      xwayland.enable = true;
      enable = true; 
      portalPackage = pkgs.xdg-desktop-portal-hyprland;
    };
    # home manager
    home-manager.users.goat = {
      stylix.targets.hyprland.enable = true;
      wayland.windowManager.hyprland = {
        plugins = pkgs.hyprlandPlugins.hyprscroller;
      };
    };
    # system packages
    environment.systemPackages = pkgs.hyprland-protocols;
  };
}
